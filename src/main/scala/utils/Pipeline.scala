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

package utils

import chisel3._
import chisel3.util._

object PipelineConnect {
  def apply[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool) = {
    val valid = RegInit(false.B)
    when (rightOutFire) { valid := false.B }
    when (left.valid && right.ready) { valid := true.B }
    when (isFlush) { valid := false.B }

    left.ready := right.ready
    right.bits := RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //&& !isFlush
  }
}

object RetimingPipelineConnect {
  def apply[T <: Data](left: ReadyValidIO[T], right: ReadyValidIO[T],
                       forwardPathPipeline: Boolean = true,
                       backwardPathPipeline: Boolean = true,
                       fullThroughput: Boolean = true
                      ) = {
    ((forwardPathPipeline, backwardPathPipeline), fullThroughput) match {
      // Forward and backward pipeline, 100% throughput
      case ((true, true), true) =>
        throw new UnsupportedOperationException("Forward and backward pipeline, 100% throughput is not supported");

      // Forward and backward pipeline, 50% throughput
      case ((true, true), false) =>
        throw new UnsupportedOperationException("Forward and backward pipeline, 50% throughput is not supported");

      // Forward pipeline only, 100% throughput
      case ((true, false), _) =>
        val rvalid = RegInit(false.B)
        val rbits = Reg(chiselTypeOf(left.bits))

        when(left.valid) {
          rvalid := true.B
        }.elsewhen(right.ready) {
          rvalid := false.B
        }
        when(left.valid && left.ready) {
          rbits := left.bits
        }

        right.valid := rvalid
        right.bits := rbits
        left.ready := right.ready || !right.valid

      // Backward pipeline only, 100% throughput
      case ((false, true), true) =>
        throw new UnsupportedOperationException("Backward pipeline only, 100% throughput is not supported");

      // Backward pipeline only, 50% throughput
      case ((false, true), false) =>
        throw new UnsupportedOperationException("Backward pipeline only, 50% throughput is not supported");

      // Pass-through
      case ((false, false), _) =>
        // left <> right;
        right.valid := left.valid
        right.bits := left.bits
        left.ready := right.ready

      case _ =>
        throw new UnsupportedOperationException("pipeline type is not supported");
    }
  }
}
