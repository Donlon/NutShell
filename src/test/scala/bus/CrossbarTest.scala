package bus

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import bus.simplebus.SimpleBusCrossbarRRArbiter

class SimpleBusRRArbiterTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SimpleBusRRArbiter"

  it should "www" in {
    test(new SimpleBusCrossbarRRArbiter(n = 1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
    }
  }
}

class SimpleBusTesterTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SimpleBusRequester/Responser"
  // test class body here
  it should "do transactions" in {
    test(new SimpleBusDirectlyConnectedTester(addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(50000)
      c.io.reqThrottling.poke(0.U)
      c.io.respThrottling.poke(0.U)
      c.io.reqEnable.poke(true.B)
      var cycleCount = 0
      while (c.io.respCount.peek().litValue < 1000) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions with throttling" in {
    test(new SimpleBusDirectlyConnectedTester(addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(100000)
      c.io.reqThrottling.poke(12.U)
      c.io.respThrottling.poke(12.U)
      c.io.reqEnable.poke(true.B)
      var cycleCount = 0
      while (c.io.respCount.peek().litValue < 1000) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
}

class CrossbarTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SimpleBusCrossbarNto1"
  // test class body here
  it should "do transactions" in {
    test(new SimpleBusCrossbarNto1Tester(1, addrBits = 64, userBits = 0))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(10000)
      c.io.reqThrottling.poke(0.U)
      c.io.respThrottling.poke(0.U)
      c.io.reqEnable.poke(true.B)
      var cycleCount = 0
      while (c.io.respCount(0).peek().litValue < 1000) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions with throttling" in {
    test(new SimpleBusCrossbarNto1Tester(1, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(20000)
      c.io.reqThrottling.poke(12.U)
      c.io.respThrottling.poke(12.U)
      c.io.reqEnable.poke(true.B)
      var cycleCount = 0
      while (c.io.respCount.map(_.peek().litValue < 1000).reduce(_ || _)) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions (2->1)" in {
    test(new SimpleBusCrossbarNto1Tester(2, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      try {
        c.clock.setTimeout(40000)
        c.io.reqThrottling.poke(0.U)
        c.io.respThrottling.poke(0.U)
        c.io.reqEnable.poke(true.B)
        var cycleCount = 0
        while (c.io.respCount.map(_.peek().litValue < 1000).reduce(_ || _)) {
          c.clock.step()
          cycleCount += 1
        }
        Predef.printf("finished in %d cycles.\n", cycleCount)
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
  }
  it should "do transactions with throttling (2->1)" in {
    test(new SimpleBusCrossbarNto1Tester(2, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(40000)
      c.io.reqThrottling.poke(12.U)
      c.io.respThrottling.poke(12.U)
      c.io.reqEnable.poke(true.B)
      var cycleCount = 0
      while (c.io.respCount.map(_.peek().litValue < 1000).reduce(_ || _)) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions with throttling (4->1)" in {
    test(new SimpleBusCrossbarNto1Tester(4, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(50000)
      c.io.reqThrottling.poke(12.U)
      c.io.respThrottling.poke(12.U)
      c.io.reqEnable.poke(true.B)
      var cycleCount = 0
      while (c.io.respCount.map(_.peek().litValue < 1000).reduce(_ || _)) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions (8->1)" in {
    test(new SimpleBusCrossbarNto1Tester(8, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
      c.clock.setTimeout(100000)
      c.io.reqThrottling.poke(0.U)
      c.io.respThrottling.poke(0.U)
      c.io.reqEnable.poke(true.B)
      var cycleCount = 0
      while (c.io.respCount.map(_.peek().litValue < 1000).reduce(_ || _)) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions with throttling (8->1)" in {
    test(new SimpleBusCrossbarNto1Tester(8, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
      c.clock.setTimeout(100000)
      c.io.reqThrottling.poke(12.U)
      c.io.respThrottling.poke(12.U)
      c.io.reqEnable.poke(true.B)
      var cycleCount = 0
      while (c.io.respCount.map(_.peek().litValue < 1000).reduce(_ || _)) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
}