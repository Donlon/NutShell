package bus

import chisel3._
import chisel3.util._
import bus.simplebus._

object MyPRNG {
  class PRNG(width: Int, seed: Option[BigInt] = Some(1)) {
    val current = RegInit(seed.get.U(width.W))
    val nextValue = nextRand(current, width)
    val isNext = WireInit(false.B)

    when(isNext) {
      current := nextValue
    }

    def keep(): Unit = isNext := false.B

    def next(): Unit = isNext := true.B
  }

  def apply(width: Int, seed: Option[BigInt] = Some(1)) = new PRNG(width, seed)

  def nextRand(current: UInt, width: Int) = {
    val next = current * 134775813.U + 1.U
    next(width - 1, 0)
  }
}

private object getExpectedReadData {
  def apply(addr: UInt, width: Int) = addr ^ Fill(width, true.B)
}

private object RequestType {
  val read :: readBurst :: write :: writeBurst :: nil = Enum(4)

  def get(addr: UInt, width: Int) = addr(width - 1, width - 2)
}

class SimpleBusRequesterGen(seed: Int, addrBits: Int, userBits: Int) extends Module {
  val LineBeats = 8 //DATA WIDTH 64

  val req = IO(Decoupled(new SimpleBusReqBundle(userBits, addrBits)))
  val enable = IO(Input(Bool()))
  val reqThrottling = IO(Input(UInt(16.W)))

  val burstWriteIndex = RegInit(0.U(3.W))

  val reqValid = RegInit(false.B)
  // val reqCmd = Reg(chiselTypeOf(req.bits.cmd))
  // val reqData = Reg(chiselTypeOf(req.bits.wdata))
  val reqCmd = Wire(SimpleBusCmd())
  val reqData = Wire(chiselTypeOf(req.bits.wdata))
  val reqAddr = MyPRNG(addrBits, Some(seed))
  val reqType = RequestType.get(reqAddr.current, addrBits)
  val nextReqType = RequestType.get(reqAddr.nextValue, addrBits)

  val reqThrottlingRNG = MyPRNG(32, Some(seed))
  val reqThrottlingCycles = ((reqThrottling * reqThrottlingRNG.current) >> 32).asUInt()(15, 0)
  val reqThrottlingCounter = RegInit(0.U(16.W))

  val lastReqData = RegEnable(next = reqData, enable = req.fire)

  reqCmd := SimpleBusCmd.read
  reqData := 0.U

  req.valid := reqValid
  req.bits.apply(addr = reqAddr.current, cmd = reqCmd, size = "b10".U, wdata = reqData, wmask = 0xff.U)

  when(req.fire) {
    when(reqCmd === SimpleBusCmd.writeBurst) {
      burstWriteIndex := burstWriteIndex + 1.U
    }.elsewhen(reqCmd === SimpleBusCmd.writeLast) {
      burstWriteIndex := 0.U
    }
  }
  val expectedReqData: UInt = dontTouch(getExpectedReadData(reqAddr.current, addrBits))

  when(req.fire || !reqValid) {
    when(reqThrottlingCounter === 0.U && enable) {
      reqValid := true.B
    }.otherwise {
      reqValid := false.B
    }
  }
  when(!reqValid && reqThrottlingCounter =/= 0.U) {
    reqThrottlingCounter := reqThrottlingCounter - 1.U
  }
  when(req.fire) {
    reqThrottlingRNG.next()
    reqThrottlingCounter := reqThrottlingCycles
  }

  // TODO: use registered output for better readability
  switch(reqType) {
    is(RequestType.read) {
      reqCmd := SimpleBusCmd.read
      reqData := 0.U
    }
    is(RequestType.readBurst) {
      reqCmd := SimpleBusCmd.readBurst
      reqData := 0.U
    }
    is(RequestType.write) {
      reqCmd := SimpleBusCmd.write
      reqData := getExpectedReadData(reqAddr.current, addrBits)
    }
    is(RequestType.writeBurst) {
      when(burstWriteIndex =/= (LineBeats - 1).U) {
        reqCmd := SimpleBusCmd.writeBurst
      }.otherwise {
        reqCmd := SimpleBusCmd.writeLast
      }
      when(burstWriteIndex === 0.U) {
        reqData := getExpectedReadData(reqAddr.current, addrBits)
      }.otherwise {
        reqData := MyPRNG.nextRand(lastReqData, addrBits)
      }
    }
  }
  // TODO: increase reqRandAddr if is burst write

  when(req.fire && req.bits.cmd =/= SimpleBusCmd.writeBurst) {
    reqAddr.next()
  }
}

class SimpleBusRequesterMon(MonitorID: Int, seed: Int, addrBits: Int, userBits: Int) extends Module {
  val LineBeats = 8 // DATA WIDTH 64

  val resp = IO(Flipped(Decoupled(new SimpleBusRespBundle(userBits))))
  val respThrottling = IO(Input(UInt(16.W)))

  val respReady = RegInit(false.B)
  val respThrottlingRNG = MyPRNG(32, Some(seed))
  val respThrottlingCycles = ((respThrottling * respThrottlingRNG.current) >> 32).asUInt()(15, 0)
  val respThrottlingCounter = RegInit(0.U(16.W))

  resp.ready := respThrottlingCounter === 0.U

  when(!respReady && respThrottlingCounter =/= 0.U) {
    respThrottlingCounter := respThrottlingCounter - 1.U
  }
  when(resp.fire) {
    respThrottlingRNG.next()
    respThrottlingCounter := respThrottlingCycles
  }

  val isResponseDone = Wire(Bool())
  val expectedReadAddr = MyPRNG(addrBits, Some(seed))
  val expectedReqType = RequestType.get(expectedReadAddr.current, addrBits)

  val burstReadIndex = RegInit(0.U(3.W))

  val lastRespData = RegEnable(next = resp.bits.rdata, enable = resp.fire)

  isResponseDone := resp.fire && (resp.bits.isReadLast() || resp.bits.isWriteResp())
  when(isResponseDone) {
    expectedReadAddr.next()
  }
  // val reqRandData = MyPRNG(addrBits, Some(SEED), startReq)

  when(resp.fire) {
    switch(expectedReqType) {
      is(RequestType.read) {
        // when(resp.bits.rdata =/= getExpectedReadData(respRandReadAddr, addrBits)) {
        //   printf("Error: resp.data %x != expected %x\n", resp.bits.rdata, getExpectedReadData(respRandReadAddr, addrBits))
        // }
        assert(resp.bits.cmd === SimpleBusCmd.readLast,
          "Unexpected resp.cmd %d", resp.bits.cmd)
        assert(resp.bits.rdata === getExpectedReadData(expectedReadAddr.current, addrBits),
          "Unexpected read data 0x%x. expected=0x%x, addr=0x%x",
          resp.bits.rdata, getExpectedReadData(expectedReadAddr.current, addrBits), expectedReadAddr.current)
      }
      is(RequestType.readBurst) {
        when(burstReadIndex =/= (LineBeats - 1).U) {
          assert(resp.bits.cmd === SimpleBusCmd.readBurst,
            "Monitor %d: Unexpected resp.cmd %d, addr=0x%x",
            MonitorID.U, resp.bits.cmd, expectedReadAddr.current)
          burstReadIndex := burstReadIndex + 1.U
        }.otherwise {
          assert(resp.bits.cmd === SimpleBusCmd.readLast,
            "Unexpected resp.cmd %d", resp.bits.cmd)
          burstReadIndex := 0.U
        }
        when(burstReadIndex === 0.U) {
          assert(resp.bits.rdata === getExpectedReadData(expectedReadAddr.current, addrBits))
        }.otherwise {
          assert(resp.bits.rdata === MyPRNG.nextRand(lastRespData, addrBits))
        }
      }
      is(RequestType.write) {
        assert(resp.bits.cmd === SimpleBusCmd.writeResp,
          "Unexpected resp.cmd %d", resp.bits.cmd)
      }
      is(RequestType.writeBurst) {
        assert(resp.bits.cmd === SimpleBusCmd.writeResp,
          "Unexpected resp.cmd %d", resp.bits.cmd)
      }
    }
  }
  // TODO: monitor burst read response len
}

class SimpleBusRequester(RequesterId: Int, seed: Int, addrBits: Int, userBits: Int) extends Module {
  val outBus = IO(new SimpleBusUC(userBits, addrBits))
  val reqEnable = IO(Input(Bool()))
  val reqCount = IO(Output(UInt()))
  val respCount = IO(Output(UInt()))
  val reqThrottling = IO(Input(UInt(16.W)))
  val respThrottling = IO(Input(UInt(16.W)))

  val gen = Module(new SimpleBusRequesterGen(seed, addrBits, userBits))
  val mon = Module(new SimpleBusRequesterMon(RequesterId, seed, addrBits, userBits))
  gen.enable := reqEnable
  gen.reqThrottling := reqThrottling
  mon.respThrottling := respThrottling
  reqCount := Counter(gen.req.fire && gen.req.bits.cmd =/= SimpleBusCmd.writeBurst, 65536)._1
  respCount := Counter(mon.resp.fire && mon.resp.bits.cmd =/= SimpleBusCmd.read, 65536)._1
  outBus.req <> gen.req
  outBus.resp <> mon.resp
}

class SimpleBusResponserCore(addrBits: Int, userBits: Int) extends Module {
  val LineBeats = 8 // DATA WIDTH 64

  val inBus = IO(Flipped(new SimpleBusUC(userBits, addrBits)))

  val respValid = RegInit(false.B)
  val respCmd = Reg(chiselTypeOf(inBus.resp.bits.cmd))
  val respData = Reg(chiselTypeOf(inBus.resp.bits.rdata))

  val burstReadIndex = RegInit(0.U(3.W))
  val burstWriteIndex = RegInit(0.U(3.W))

  val lastWriteData = RegEnable(next = inBus.req.bits.wdata, enable = inBus.req.fire)

  // Request processing
  inBus.req.ready := !(inBus.resp.valid && (!inBus.resp.ready || inBus.resp.bits.cmd === SimpleBusCmd.readBurst))
  // TODO: req.ready throttling

  // Response processing
  inBus.resp.valid := respValid
  inBus.resp.bits := DontCare
  inBus.resp.bits.cmd := respCmd
  inBus.resp.bits.rdata := respData

  when(inBus.resp.fire) {
    respValid := false.B
  }
  when(inBus.req.fire) { //req.ready should be deasserted when resp=read
    when(inBus.req.bits.isRead()) {
      respValid := true.B
      when(inBus.req.bits.isReadBurst()) {
        respCmd := SimpleBusCmd.readBurst
      }.otherwise {
        respCmd := SimpleBusCmd.readLast
      }
      respData := getExpectedReadData(inBus.req.bits.addr, addrBits)
      // burstReadIndex := 0.U
    }.elsewhen(inBus.req.bits.isWriteSingle()) {
      respValid := true.B
      respCmd := SimpleBusCmd.writeResp
      assert(inBus.req.bits.wdata === getExpectedReadData(inBus.req.bits.addr, addrBits),
        "Unexpected burst written data. received=0x%x, expected=0x%x",
        inBus.req.bits.wdata, getExpectedReadData(inBus.req.bits.addr, addrBits))
    }.elsewhen(inBus.req.bits.cmd === SimpleBusCmd.writeBurst) {
      when(burstWriteIndex === 0.U) {
        assert(inBus.req.bits.wdata === getExpectedReadData(inBus.req.bits.addr, addrBits),
          "Unexpected burst written data. received=0x%x, expected=0x%x",
          inBus.req.bits.wdata, getExpectedReadData(inBus.req.bits.addr, addrBits))
        // reqWriteData := getExpectedReadData(reqRandAddr, addrBits)
      }.otherwise {
        assert(inBus.req.bits.wdata === MyPRNG.nextRand(lastWriteData, addrBits),
          "Unexpected burst written data. received=0x%x, expected=0x%x",
          inBus.req.bits.wdata, MyPRNG.nextRand(lastWriteData, addrBits))
      }
      burstWriteIndex := burstWriteIndex + 1.U
    }.elsewhen(inBus.req.bits.isWriteLast()) {
      respValid := true.B
      respCmd := SimpleBusCmd.writeResp
      respData := 0.U
      assert(inBus.req.bits.wdata === MyPRNG.nextRand(lastWriteData, addrBits))
      // assert(burstWriteIndex === (LineBeats - 1).U)
      burstWriteIndex := 0.U
    }
    when (burstWriteIndex =/= 0.U) {
      when (burstWriteIndex === (LineBeats - 1).U) {
        assert(inBus.req.bits.cmd === SimpleBusCmd.writeLast)
      }.otherwise{
        assert(inBus.req.bits.cmd === SimpleBusCmd.writeBurst)
      }
    }
  }
  when(inBus.resp.fire && inBus.resp.bits.isRead()) {
    when(!inBus.resp.bits.isReadLast()) {
      // burst read: continue data transmission
      respValid := true.B
      respData := MyPRNG.nextRand(inBus.resp.bits.rdata, addrBits)
      burstReadIndex := burstReadIndex + 1.U
    }.otherwise { // when(burstReadIndex === (LineBeats - 1).U)
      burstReadIndex := 0.U
    }
    when(burstReadIndex === (LineBeats - 2).U) {
      respCmd := SimpleBusCmd.readLast
    }
  }
}

class SimpleBusResponser(addrBits: Int, userBits: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC(userBits, addrBits))
    val reqCount = Output(UInt(32.W))
    val respCount = Output(UInt(32.W))
  })
  val responserCore = Module(new SimpleBusResponserCore(addrBits, userBits))

  val reqCount = RegInit(0.U(32.W))
  val respCount = RegInit(0.U(32.W))
  io.reqCount := reqCount
  io.respCount := respCount

  val delayedBus_q1 = io.in.retiming(reqForwardPath = true)
  val delayedBus_q2 = delayedBus_q1.retiming(reqForwardPath = true)
  val delayedBus = delayedBus_q2.retiming(reqForwardPath = true)
  responserCore.inBus <> delayedBus

  when(io.in.req.fire) {
    reqCount := reqCount + 1.U
  }
  when(io.in.resp.fire) {
    respCount := respCount + 1.U
  }
}

class SimpleBusDirectlyConnectedTester(addrBits: Int, userBits: Int) extends Module {
  val io = IO(new Bundle {
    val reqEnable = Input(Bool())
    val reqCount = Output(UInt())
    val respCount = Output(UInt())
    val reqThrottling = Input(UInt(16.W))
    val respThrottling = Input(UInt(16.W))
  })
  val SEED = 0x514
  val requester = Module(new SimpleBusRequester(0, SEED, addrBits, userBits))
  val responser = Module(new SimpleBusResponser(addrBits, userBits))
  requester.outBus <> responser.io.in
  requester.reqEnable := io.reqEnable
  requester.reqThrottling := io.reqThrottling
  requester.respThrottling := io.respThrottling

  io.reqCount := requester.reqCount
  io.respCount := requester.respCount
}

class SimpleBusCrossbarNto1Tester(n: Int, addrBits: Int, userBits: Int) extends Module {
  val io = IO(new Bundle {
    val reqEnable = Input(Bool())
    val reqCount = Output(Vec(n, UInt()))
    val respCount = Output(Vec(n, UInt()))
    val reqThrottling = Input(UInt(16.W))
    val respThrottling = Input(UInt(16.W))
  })
  val SEED = 0x514
  val requesters = List.tabulate(n)(i => Module(new SimpleBusRequester(i, (SEED + i) * (i + 1), addrBits, userBits)))
  val responser = Module(new SimpleBusResponser(addrBits, userBits))
  val xbar = Module(new SimpleBusCrossbarNto1(n = n, userBits = userBits, addrBits = addrBits))
  (requesters zip xbar.io.in).foreach { case (left, right) =>
    left.reqEnable := io.reqEnable
    left.outBus <> right
    left.reqThrottling := io.reqThrottling
    left.respThrottling := io.respThrottling
  }
  xbar.io.out <> responser.io.in

  io.reqCount := requesters.map(r => r.reqCount)
  io.respCount := requesters.map(r => r.respCount)
}