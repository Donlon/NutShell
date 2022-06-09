package bus

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SimpleBusTesterTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SimpleBusRequester/Responser"
  // test class body here
  it should "do transactions" in {
    // test case body here
    test(new SimpleBusDirectlyConnectedTester(addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.clock.setTimeout(10000)
      var cycleCount = 0
      while (c.io.respCount.peek().litValue < 1000) {
        c.clock.step()
        cycleCount += 1
      }
      printf("finished in %d cycles.\n", cycleCount)
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
      var cycleCount = 0
      while (c.io.respCount.peek().litValue < 1000) {
        c.clock.step()
        cycleCount += 1
      }
      printf("finished in %d cycles.\n", cycleCount)
    }
  }
}