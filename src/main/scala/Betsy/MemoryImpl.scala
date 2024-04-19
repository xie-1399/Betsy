package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/19      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 **
 ** **
 */

import spinal.core._
import spinal.lib._

object MemoryImpl extends SpinalEnum{
  val SpinalMem,SRAM,BlockRAM = newElement()
  defaultEncoding = SpinalEnumEncoding("MemoryImpl")(
    SpinalMem -> 0,   // Register Bank
    BlockRAM -> 1,   // FPGA
    SRAM -> 2
  )
}

class MemoryImpl {

}
