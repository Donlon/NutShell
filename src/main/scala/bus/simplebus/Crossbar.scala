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

import utils._

// This module does not support burst transaction
class SimpleBusCrossbar1toN(addressSpace: List[List[(Long, Long)]]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = Vec(addressSpace.length, new SimpleBusUC)
  })

  val s_idle :: s_resp :: s_error :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // select the output channel according to the address
  val addr = io.in.req.bits.addr
  val outMatchVec = addressSpace.map(_.map(IsAddressMatched(addr, _)).reduce(_ || _))
  val outSelVec = VecInit(PriorityEncoderOH(outMatchVec))
  val outSelRespVec = RegEnable(next=outSelVec,
                                init=VecInit(Seq.fill(outSelVec.length)(false.B)),
                                enable=io.in.req.fire && state === s_idle)
  val reqInvalidAddr = io.in.req.valid && !outMatchVec.reduce(_ || _)

  when (reqInvalidAddr) {
    Debug() {
      printf("crossbar access bad addr %x, time %d\n", addr, GTimer())
    }
  }
  assert(!reqInvalidAddr, "address decode error, bad addr = 0x%x\n", addr)

  switch (state) {
    is (s_idle) {
      when (io.in.req.fire) { state := s_resp }
      when (reqInvalidAddr) { state := s_error }
    }
    is (s_resp) { when (io.in.resp.fire) { state := s_idle } } // TODO: isReadLast
    is (s_error) { when (io.in.resp.fire) { state := s_idle } }
  }

  // bind out.req channel
  io.in.req.ready := Mux1H(outSelVec, io.out.map(_.req.ready)) || reqInvalidAddr
  for (i <- 0 until io.out.length) {
    io.out(i).req.valid := outSelVec(i) && io.in.req.valid && state === s_idle
    io.out(i).req.bits := io.in.req.bits
  }

  // bind in.resp channel
  for (i <- 0 until io.out.length) {
    io.out(i).resp.ready := outSelRespVec(i) && io.in.resp.ready && state === s_resp
  }
  io.in.resp.valid := Mux1H(outSelRespVec, io.out.map(_.resp.valid)) || state === s_error
  io.in.resp.bits := Mux1H(outSelRespVec, io.out.map(_.resp.bits))
  // io.in.resp.bits.exc.get := state === s_error

  Debug() {
    when (io.in.req.fire) {
      printf(p"${GTimer()}: xbar: outSelVec = ${outSelVec}, outSel.req: ${io.in.req.bits}\n")
    }
    when (io.in.resp.fire) {
      printf(p"${GTimer()}: xbar: outSelVec = ${outSelVec}, outSel.resp: ${io.in.resp.bits}\n")
    }
  }
}

class SimpleBusCrossbarRRArbiter(n: Int) extends Module {
  val reqValid = IO(Input(Vec(n, Bool())))
  val respReady = IO(Input(Bool()))
  val wantsLock = IO(Input(Bool()))
  val grantVec = IO(Output(Vec(n, Bool())))
  val hasRequest = IO(Output(Bool()))

  if (n > 1) {
    hasRequest := reqValid.reduce(_ || _)
    val lastGrant = RegInit(1.U(n.W))
    val lastGrantRotated = Cat(lastGrant(n - 2, 0), lastGrant(n - 1))

    val reqValidCat = VecInit(reqValid ++ reqValid).asUInt
    val preGrant_n = (~reqValidCat).asUInt | (reqValidCat - lastGrantRotated)
    grantVec := Mux(wantsLock, VecInit(lastGrant.asBools), VecInit(Seq.tabulate(n)(i => !(preGrant_n(i) && preGrant_n(n + i)))))

    when(hasRequest && respReady && !wantsLock) {
      lastGrant := grantVec.asUInt
    }

    reqValidCat.suggestName("reqValidCat")
    preGrant_n.suggestName("preGrant_n")
  } else {
    grantVec := VecInit(true.B)
    hasRequest := reqValid(0)
  }
}

class SimpleBusCrossbarNto1(n: Int,
                            userBits: Int = 0,
                            addrBits: Int = 32,
                            maxOutstandingTransactions: Int = 4) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new SimpleBusUC(userBits, addrBits)))
    val out = new SimpleBusUC(userBits, addrBits)
  })

  val enqueueGrantVec = Wire(new IrrevocableIO(Vec(n, Bool())))
  val dequeuedGrantVec = Queue(enq = enqueueGrantVec,
    entries = maxOutstandingTransactions).suggestName("sourceInfoQueue")

  // Request channel processing
  val wantsLock = RegInit(false.B)
  val grantVec = Wire(Vec(n, Bool()))

  val reqArbiter = Module(new SimpleBusCrossbarRRArbiter(n))
  reqArbiter.reqValid := io.in.map(_.req.valid)
  reqArbiter.respReady := io.out.resp.ready && enqueueGrantVec.ready
  reqArbiter.wantsLock := wantsLock
  grantVec := reqArbiter.grantVec

  val queueReady = enqueueGrantVec.ready || wantsLock

  io.out.req.valid := Mux1H(grantVec, io.in.map(_.req.valid)) && queueReady
  io.out.req.bits := Mux1H(grantVec, io.in.map(_.req.bits))
  (io.in zip grantVec).foreach { case (inBus, grant) => inBus.req.ready := grant && io.out.req.ready && queueReady}

  enqueueGrantVec.valid := reqArbiter.hasRequest && io.out.req.ready && !wantsLock // io.out.req.fire && !wantsLock
  enqueueGrantVec.bits := grantVec

  when (io.out.req.fire && enqueueGrantVec.ready) {
    wantsLock := io.out.req.bits.cmd === SimpleBusCmd.writeBurst
  }

  // Response channel processing
  (io.in zip dequeuedGrantVec.bits).foreach { case (inBus, grant) =>
    inBus.resp.valid := dequeuedGrantVec.valid && grant && io.out.resp.valid
    inBus.resp.bits := io.out.resp.bits
  }
  io.out.resp.ready := Mux1H(dequeuedGrantVec.bits, io.in.map(_.resp.ready))
  dequeuedGrantVec.ready := io.out.resp.fire && io.out.resp.bits.cmd =/= SimpleBusCmd.readBurst
  // TODO: is burst read
}

class SimpleBusCrossbar(n: Int, addressSpace: List[List[(Long, Long)]]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new SimpleBusUC))
    val out = Vec(addressSpace.length, new SimpleBusUC)
  })

  val inXbar = Module(new SimpleBusCrossbarNto1(n))
  val outXbar = Module(new SimpleBusCrossbar1toN(addressSpace))
  inXbar.io.in <> io.in
  outXbar.io.in <> inXbar.io.out
  io.out <> outXbar.io.out
}

class SimpleBusAutoIDCrossbarNto1(n: Int, userBits: Int = 0) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new SimpleBusUC(userBits)))
    val out = new SimpleBusUC(userBits, idBits = n)
  })

  // Note: to use SimpleBusAutoIDCrossbarNto1, every master device must ensure resp.ready is always true 

  val reqValid = WireInit(VecInit(List.tabulate(n)(i => io.in(i).req.valid)))
  val reqSelect = PriorityEncoder(reqValid.asUInt)
  val reqSelectVec = PriorityEncoderOH(reqValid.asUInt)

  // ID will be automatically assigned for master devices
  // TODO: use Mux1H?
  io.out.req.bits.addr := io.in(reqSelect).req.bits.addr
  io.out.req.bits.size := io.in(reqSelect).req.bits.size
  io.out.req.bits.cmd := io.in(reqSelect).req.bits.cmd
  io.out.req.bits.wmask := io.in(reqSelect).req.bits.wmask
  io.out.req.bits.wdata := io.in(reqSelect).req.bits.wdata
  if(userBits > 0){
    io.out.req.bits.user.get := io.in(reqSelect).req.bits.user.get
  }

  io.out.req.valid := reqValid.asUInt.orR
  io.out.req.bits.id.get := reqSelectVec.asUInt // Simple bus ID is OH 
  io.out.resp.ready := true.B // io.in(reqSelect).resp.ready
  // assert(io.out.resp.ready)

  for(i <- 0 until n){
    io.in(i).req.ready := io.out.req.ready && reqSelectVec(i)
    io.in(i).resp.valid := io.out.resp.valid && io.out.resp.bits.id.get(i)
    io.in(i).resp.bits.cmd := io.out.resp.bits.cmd
    io.in(i).resp.bits.rdata := io.out.resp.bits.rdata
    if(userBits > 0){
      io.in(i).resp.bits.user.get := io.out.resp.bits.user.get
    }
  }

  Debug(){
    when(io.out.req.fire){
      printf("[Crossbar REQ] addr %x cmd %x select %b\n", io.out.req.bits.addr, io.out.req.bits.cmd, reqSelectVec)
    }
    when(io.out.resp.fire){
      printf("[Crossbar RESP] data %x select %b\n", io.out.resp.bits.rdata, io.out.resp.bits.id.get)
    }
  }

}
