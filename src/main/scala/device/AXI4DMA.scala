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

package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import utils._
import nutcore._

class DMABundle extends Bundle {
  val dma = new AXI4
}

class AXI4DMA extends AXI4SlaveModule(new AXI4Lite, new DMABundle) with HasNutCoreParameter with HasNutCoreLog{
  val step = 128 // unit: byte. step = 8 is not supported now.
  val stepBits = step * 8
  val LineBytes = 8
  val autoStart = false

  val dest = Reg(UInt(64.W))
  val src = Reg(UInt(64.W))
  val len = RegInit(0.U(64.W))

  val dma = io.extra.get.dma
  //val data = Reg(UInt(stepBits.W))
  val data = RegInit(VecInit(Seq.fill(LineBytes * 2)(0.U(64.W))))

  val s_idle :: s_read_req :: s_read_wait_resp :: s_write_req :: s_write_wait_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  if (autoStart) {
    when (state === s_idle && len === 0.U) {
      len := 32768.U
      dest := "h62000000".U
      src  := "h62000000".U
      printf("\n@")
    }
  }
  val read_beat = RegInit(0.U(4.W))
  when (state === s_idle && len =/= 0.U) { state := s_read_req }
  when (state === s_read_req && dma.ar.fire()) { state := s_read_wait_resp }
  when (state === s_read_wait_resp && dma.r.fire()) {
    read_beat := read_beat + 1.U
    data(read_beat) := dma.r.bits.data
    when (read_beat === 15.U) {
      state := s_write_req
      read_beat := 0.U
    }
  }

  val wSend = Wire(Bool())
  val wlast = dma.w.bits.last
  val awAck = BoolStopWatch(dma.aw.fire(), wSend)
  val wAck = BoolStopWatch(dma.w.fire() && wlast, wSend)
  wSend := (dma.aw.fire() && dma.w.fire() && wlast) || (awAck && wAck)

  val write_beat = RegInit(0.U(4.W))

  when (state === s_write_req && wSend) { state := s_write_wait_resp }
  when (state === s_write_wait_resp && dma.b.fire()) {
    len := len - step.U
    dest := dest + step.U
    src := src + step.U
    state := Mux(len <= step.U, s_idle, s_read_req)
    write_beat := 0.U
  }

  when (state === s_write_req && dma.w.fire()) {
    write_beat := write_beat + 1.U
  }

  dma.ar.bits.prot := AXI4Parameters.PROT_PRIVILEDGED
  dma.ar.bits.id := 0.U
  dma.ar.bits.size := log2Ceil(LineBytes).U
  dma.ar.bits.burst := AXI4Parameters.BURST_INCR
  dma.ar.bits.lock := false.B
  dma.ar.bits.cache := 0.U
  dma.ar.bits.qos := 0.U
  dma.ar.bits.user := 0.U
  dma.ar.bits.len := 15.U   //8 * 8
  dma.ar.bits.addr := src
  dma.ar.valid := (state === s_read_req)
  dma.r.ready := (state === s_read_wait_resp)

  dma.aw.bits := dma.ar.bits
  dma.aw.bits.addr := dest
  dma.aw.valid := (state === s_write_req) && !awAck
  dma.w.valid := (state === s_write_req) && !wAck
  dma.w.bits.data := data(write_beat)
  dma.w.bits.strb := Fill(LineBytes, "b1".U)
  dma.w.bits.last := write_beat === 15.U
  dma.b.ready := (state === s_write_wait_resp) || ((state === s_write_req) && !wAck)

  val mapping = Map(
    RegMap(0x0, dest),
    RegMap(0x8, src),
    RegMap(0x10, len)
  )

  RegMap.generate(mapping, raddr(4,0), in.r.bits.data,
    waddr(4,0), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2,0)))

  Debug(len =/= 0.U && state === s_idle, "dma len not 0!\n")
}

