/**************************************************************************************
* Copyright (c) 2022 Institute of Computing Technology, CAS
* Copyright (c) 2022 University of Chinese Academy of Sciences
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
import nutcore.FuType.PAddrBits

// IsAddressInRange
object IsAddressMatched {
  def apply(address: UInt, range: (Long, Long), addressBits : Int = PAddrBits) = {
    val rangeStart = range._1
    val rangeLength = range._2
    if (isPow2(rangeLength) && (rangeStart & (rangeLength - 1)) == 0) {
      val rangeLengthBits = log2Up(rangeLength)
      address(addressBits - 1, rangeLengthBits) === rangeStart.U(addressBits - 1, rangeLengthBits)
    } else {
      println("[warn] Matching for address range start=0x" + rangeStart.toHexString + ", length=0x" + rangeLength.toHexString + " is not optimized")
      address >= range._1.U && address < (range._1 + range._2).U
    }
  }
}


