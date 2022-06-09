package bus

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SimpleBusTesterTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SimpleBusRequester/Responser"
  // test class body here
  it should "do transactions" in {
    // test case body here
    test(new SimpleBusDirectlyConnectedTester(addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(10000)
      c.io.reqThrottling.poke(0.U)
      c.io.respThrottling.poke(0.U)
      var cycleCount = 0
      while (c.io.respCount.peek().litValue < 1000) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions with throttling" in {
    // test case body here
    test(new SimpleBusDirectlyConnectedTester(addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(10000)
      c.io.reqThrottling.poke(16.U)
      c.io.respThrottling.poke(16.U)
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
    // test case body here
    test(new SimpleBusCrossbarNto1Tester(1, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(10000)
      c.io.reqThrottling.poke(0.U)
      c.io.respThrottling.poke(0.U)
      var cycleCount = 0
      while (c.io.respCount(0).peek().litValue < 1000) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions with throttling" in {
    // test case body here
    test(new SimpleBusCrossbarNto1Tester(1, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(10000)
      c.io.reqThrottling.poke(16.U)
      c.io.respThrottling.poke(16.U)
      var cycleCount = 0
      while (c.io.respCount.map(_.peek().litValue < 1000).reduce(_ && _)) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
  it should "do transactions (2->1)" in {
    // test case body here
    test(new SimpleBusCrossbarNto1Tester(2, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(10000)
      c.io.reqThrottling.poke(0.U)
      c.io.respThrottling.poke(0.U)
      var cycleCount = 0
      while (c.io.respCount.map(_.peek().litValue < 1000).reduce(_ && _)) {
        c.clock.step()
        cycleCount += 1
      }
      Predef.printf("finished in %d cycles.\n", cycleCount)
    }
  }
}