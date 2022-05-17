/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package bus.simplebus

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class AXI42SimpleBusConverter() extends Module {
  val idBits = 18
  val io = IO(new Bundle {
    val in = Flipped(new AXI4(idBits = idBits))
    val out = new SimpleBusUC()
  })

  val (axi, mem) = (io.in, io.out)
  val (ar, aw, w, r, b) = (axi.ar.bits, axi.aw.bits, axi.w.bits, axi.r.bits, axi.b.bits)
  val (req, resp) = (mem.req.bits, mem.resp.bits)

  // Default value

  val inflight_id_reg = RegInit(0.U)
  val axi_na :: axi_read :: axi_write :: axi_read_noall :: axi_write_noall :: Nil = Enum(5)
  val inflight_type = RegInit(axi_na)
  private def setState(axi_type: UInt, id: UInt) = {
    inflight_id_reg := id
    inflight_type := axi_type;
  }
  private def resetState() = {
    inflight_type := axi_na
    inflight_id_reg := 0.U
  }
  private def isState(state: UInt) = {
    inflight_type === state
  }
  private def isInflight() = {
    !isState(axi_na)
  }
  private def isReadNoBurstAll() = {
    isState(axi_read_noall)      //not burst all
  }

  private def isWriteNoBurstAll() = {
    isState(axi_write_noall)      //not burst all
  }

  private def resetNoBurstAll() = {
    inflight_type := axi_read_noall
  }

  private def OneTransferSize(len: UInt): UInt = {
    Mux(len === 0.U, 1.U, 8.U)
  }

  private def isSimpleBurstAlign(len: UInt): Bool = {     //len : 8 对齐
    ((len >> 3) << 3) === len
  }
  // Default
  val default_mem = 0.U.asTypeOf(new SimpleBusUC)
  val default_axi = 0.U.asTypeOf(new AXI4)
  req := default_mem.req.bits
  r := default_axi.r.bits
  b := default_axi.b.bits

  //将超过8的BURST len拆分为若干个burst len为8的section
  val already_burst_len = RegInit(0.U(8.W))
  val ar_reg = Reg(new AXI4BundleA(idBits))

  // Read Path:第一种：完全重新接收一个命令    第二种：一个8len burst没做完
  when (!isInflight() && axi.ar.valid) {
    mem.req.valid := true.B
    req.addr := ar.addr
    req.cmd := Mux(ar.len === 0.U, SimpleBusCmd.read, SimpleBusCmd.readBurst)
    // TODO: consider ar.burst
    req.size := ar.size
    req.user.foreach(_ := ar.user)
    req.wmask := 0.U
    req.wdata := 0.U

    when (mem.req.fire) {
      setState(axi_read, ar.id)
      already_burst_len := already_burst_len + OneTransferSize(ar.len)
      ar_reg := ar
    }
  }

  when (isReadNoBurstAll()) {
    mem.req.valid := true.B
    req.addr := ar_reg.addr + (already_burst_len << 3)
    req.cmd := SimpleBusCmd.readBurst
    // TODO: consider ar.burst
    req.size := ar_reg.size
    req.user.foreach(_ := ar_reg.user)
    req.wmask := 0.U
    req.wdata := 0.U

    when (mem.req.fire) {
      setState(axi_read, ar_reg.id)
      already_burst_len := already_burst_len + OneTransferSize(ar_reg.len)
    }
  }

  when (isState(axi_read) && mem.resp.valid) {
    axi.r.valid := true.B
    r.data := resp.rdata
    r.id := inflight_id_reg
    // TODO: r.resp handling
    r.resp := AXI4Parameters.RESP_OKAY
    r.last := Mux(already_burst_len === (ar_reg.len + 1.U), resp.isReadLast, 0.U)
    resp.user.foreach(r.user := _)

    when (axi.r.fire && resp.isReadLast && r.last) {
      resetState()
      already_burst_len := 0.U
    }
    when (axi.r.fire && resp.isReadLast && ~r.last) {
      resetNoBurstAll()
    }
  }

  // Write Path
  val aw_reg = Reg(new AXI4BundleA(idBits))
  val bresp_en = RegInit(false.B)

  val write_beat = RegInit(0.U(3.W))

  when (!isInflight() && axi.aw.valid && !axi.ar.valid) {
    aw_reg := aw

    when (axi.aw.fire) {
      setState(axi_write, aw.id)
    }
  }

  when (isState(axi_write) && axi.w.fire()) {
    mem.req.valid := true.B
    req.cmd := Mux(aw_reg.len === 0.U, SimpleBusCmd.write,
      Mux(write_beat === 7.U, SimpleBusCmd.writeLast, SimpleBusCmd.writeBurst))
    req.addr := aw_reg.addr + (already_burst_len << 3)
    req.size := aw_reg.size
    req.wmask := w.strb
    req.wdata := w.data
    req.user.foreach(_ := aw_reg.user)
    write_beat := write_beat + 1.U

    when (w.last) {
      bresp_en := true.B
    }
    when (req.cmd === SimpleBusCmd.writeLast) {
      already_burst_len := already_burst_len + OneTransferSize(aw_reg.len)
      when (~w.last) {
        inflight_id_reg := axi_write_noall
      }
    }
  }

  when (axi.b.fire) {
    bresp_en := false.B
    resetState()
    write_beat := 0.U
    already_burst_len := 0.U
  }

  when (isWriteNoBurstAll()) {
    write_beat := 0.U
    when (mem.resp.fire()) {
      inflight_id_reg := axi_write
    }
  }



  // Arbitration
  // Slave's ready maybe generated according to valid signal, so let valid signals go through.
  mem.req.valid := (!isInflight() && axi.ar.valid) || (isState(axi_write) && axi.w.valid) || isState(axi_read_noall)
  //?
  mem.resp.ready := !isInflight() || isReadNoBurstAll() || (isState(axi_read) && axi.r.ready) || (isState(axi_write) && axi.b.ready) || isWriteNoBurstAll()
  axi.ar.ready := !isInflight() && mem.req.ready
  axi.r.valid := isState(axi_read) && mem.resp.valid && isState(axi_read)
  // AW should be buffered so no ready is considered.
  axi.aw.ready := !isInflight() && !axi.ar.valid
  axi.w.ready  := isState(axi_write) && mem.req.ready
  axi.b.valid := bresp_en && mem.resp.valid
  axi.b.bits.resp := AXI4Parameters.RESP_OKAY

  when (axi.ar.fire()) { assert(mem.req.fire() && (!isInflight() || isReadNoBurstAll())); }
  when (axi.aw.fire()) { assert(!isInflight()); }
  when (axi.w.fire()) { assert(mem.req .fire() && isState(axi_write)); }
  when (axi.b.fire()) { assert(mem.resp.fire() && (isState(axi_write) || isWriteNoBurstAll())); }
  when (axi.r.fire()) { assert(mem.resp.fire() && isState(axi_read)); }
}


class SimpleBus2AXI4Converter[OT <: AXI4Lite](outType: OT, isFromCache: Boolean) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = Flipped(Flipped(outType))
  })

  val toAXI4Lite = !(io.in.req.valid && io.in.req.bits.isBurst()) && (outType.getClass == classOf[AXI4Lite]).B
  val toAXI4 = (outType.getClass == classOf[AXI4]).B
  assert(toAXI4Lite || toAXI4)

  val (mem, axi) = (io.in, io.out)
  val (ar, aw, w, r, b) = (axi.ar.bits, axi.aw.bits, axi.w.bits, axi.r.bits, axi.b.bits)

  ar.addr  := mem.req.bits.addr
  ar.prot  := AXI4Parameters.PROT_PRIVILEDGED
  w.data := mem.req.bits.wdata
  w.strb := mem.req.bits.wmask

  def LineBeats = 8
  val wlast = WireInit(true.B)
  val rlast = WireInit(true.B)
  if (outType.getClass == classOf[AXI4]) {
    val axi4 = io.out.asInstanceOf[AXI4]
    axi4.ar.bits.id    := 0.U
    axi4.ar.bits.len   := Mux(mem.req.bits.isBurst(), (LineBeats - 1).U, 0.U)
    axi4.ar.bits.size  := mem.req.bits.size
    axi4.ar.bits.burst := (if (isFromCache) AXI4Parameters.BURST_WRAP
                           else AXI4Parameters.BURST_INCR)
    axi4.ar.bits.lock  := false.B
    axi4.ar.bits.cache := 0.U
    axi4.ar.bits.qos   := 0.U
    axi4.ar.bits.user  := 0.U
    axi4.w.bits.last   := mem.req.bits.isWriteLast() || mem.req.bits.isWriteSingle()
    wlast := axi4.w.bits.last
    rlast := axi4.r.bits.last
  }

  aw := ar
  mem.resp.bits.rdata := r.data
  mem.resp.bits.cmd  := Mux(rlast, SimpleBusCmd.readLast, 0.U)

  val wSend = Wire(Bool())
  val awAck = BoolStopWatch(axi.aw.fire(), wSend)
  val wAck = BoolStopWatch(axi.w.fire() && wlast, wSend)
  wSend := (axi.aw.fire() && axi.w.fire() && wlast) || (awAck && wAck)
  val wen = RegEnable(mem.req.bits.isWrite(), mem.req.fire())

  axi.ar.valid := mem.isRead()
  axi.aw.valid := mem.isWrite() && !awAck
  axi.w .valid := mem.isWrite() && !wAck
  mem.req.ready  := Mux(mem.req.bits.isWrite(), !wAck && axi.w.ready, axi.ar.ready)

  axi.r.ready  := mem.resp.ready
  axi.b.ready  := mem.resp.ready
  mem.resp.valid  := Mux(wen, axi.b.valid, axi.r.valid)
}

object SimpleBus2AXI4Converter {
  def apply[OT <: AXI4Lite](in: SimpleBusUC, outType: OT, isFromCache: Boolean = false): OT = {
    val bridge = Module(new SimpleBus2AXI4Converter(outType, isFromCache))
    bridge.io.in <> in
    bridge.io.out
  }
}
