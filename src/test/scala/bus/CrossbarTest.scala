// SPDX-License-Identifier: Apache-2.0

package bus

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CrossbarTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SimpleBusCrossbarNto1"
  // test class body here
  it should "do simple transaction" in {
    // test case body here
    test(new SimpleBusCrossbarNto1Tester(1, addrBits = 64, userBits = 0)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      while (c.io.respCount.peek().litValue < 100) {
        c.clock.step()
      }
    }
  }
}