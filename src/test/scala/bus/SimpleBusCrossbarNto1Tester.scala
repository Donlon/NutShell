package bus

import chisel3._
import chisel3.util._
import bus.simplebus._

private object MyPRNG {
  def apply(width: Int,
            seed: Option[BigInt] = Some(1),
            increment: Bool = true.B) = {
    val reg = RegInit(seed.get.U(width.W))
    when(increment) {
      reg := nextRand(reg, width)
    }
    reg
  }

  def nextRand(current: UInt, width: Int) = {
    val next = current * 134775813.U + 1.U
    next
  }
}

private object getExpectedReadData {
  def apply(addr: UInt, width: Int) = addr ^ Fill(width, true.B);
}

class SimpleBusRequesterGen(addrBits: Int, userBits: Int) extends Module {
  val SEED = 0x514
  val req = IO(Decoupled(new SimpleBusReqBundle(userBits, addrBits)))

  val reqValid = RegInit(false.B)
  req.valid := reqValid

  val reqBits = Reg(chiselTypeOf(req.bits))
  req.bits := reqBits

  val startReq = Wire(Bool())
  startReq := (!reqValid || req.fire) && true.B

  val reqRandReadAddr = dontTouch(MyPRNG(addrBits, Some(SEED), startReq))

  val expectedReqData: UInt = dontTouch(getExpectedReadData(reqRandReadAddr, addrBits))
  // expectedReqData.suggestName("expectedReqData")

  when(startReq) {
    reqValid := true.B
    reqBits.apply(addr = reqRandReadAddr, cmd = SimpleBusCmd.read, size = "b10".U, wdata = 0.U, wmask = 0.U)
  }
}

class SimpleBusRequesterMon(addrBits: Int, userBits: Int) extends Module {
  val SEED = 0x514
  val resp = IO(Flipped(Decoupled(new SimpleBusRespBundle(userBits))))

  resp.ready := true.B
  val respRandReadAddr = dontTouch(MyPRNG(addrBits, Some(SEED), resp.fire))
  // val reqRandData = MyPRNG(addrBits, Some(SEED), startReq)

  when(resp.fire) {
    when(resp.bits.rdata =/= getExpectedReadData(respRandReadAddr, addrBits)) {
      printf("Error: resp.data %x != expected %x\n", resp.bits.rdata, getExpectedReadData(respRandReadAddr, addrBits))
    }
    assert(resp.bits.rdata === getExpectedReadData(respRandReadAddr, addrBits))
  }
}

class SimpleBusRequester(addrBits: Int, userBits: Int) extends Module {
  val outBus = IO(new SimpleBusUC(userBits, addrBits))
  val gen = Module(new SimpleBusRequesterGen(addrBits, userBits))
  val mon = Module(new SimpleBusRequesterMon(addrBits, userBits))
  outBus.req <> gen.req
  outBus.resp <> mon.resp
}

class SimpleBusResponser(addrBits: Int, userBits: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC(userBits, addrBits))
    val reqCount = Output(UInt(32.W))
    val respCount = Output(UInt(32.W))
  })
  val reqCount = RegInit(0.U(32.W))
  val respCount = RegInit(0.U(32.W))
  io.reqCount := reqCount
  io.respCount := respCount
  val respValid = RegInit(false.B)
  val respData = Reg(chiselTypeOf(io.in.resp.bits.rdata))

  val delayedBus = io.in.retiming(reqForwardPath = true).retiming(reqForwardPath = true).retiming(reqForwardPath = true)
  delayedBus.req.ready := delayedBus.resp.ready

  delayedBus.resp.valid := respValid
  delayedBus.resp.bits := DontCare
  delayedBus.resp.bits.rdata := respData

  when(delayedBus.resp.fire) {
    respValid := false.B
  }
  when(delayedBus.req.fire) {
    respValid := true.B
    delayedBus.resp.bits.cmd := SimpleBusCmd.readLast
    respData := getExpectedReadData(delayedBus.req.bits.addr, addrBits)
  }

  when(io.in.req.fire) {
    reqCount := reqCount + 1.U
  }
  when(io.in.resp.fire) {
    respCount := respCount + 1.U
  }
}

class SimpleBusCrossbarNto1Tester(n: Int, addrBits: Int, userBits: Int) extends Module {
  val io = IO(new Bundle {
    val reqCount = Output(UInt())
    val respCount = Output(UInt())
  })
  val requesters = List.fill(n)(Module(new SimpleBusRequester(addrBits, userBits)))
  val responser = Module(new SimpleBusResponser(addrBits, userBits))
  val xbar = Module(new SimpleBusCrossbarNto1(n = n, userBits = userBits, addrBits = addrBits))
  requesters.zip(xbar.io.in).foreach { case (left, right) => left.outBus <> right }
  xbar.io.out <> responser.io.in

  io.reqCount := responser.io.reqCount
  io.respCount := responser.io.respCount
}