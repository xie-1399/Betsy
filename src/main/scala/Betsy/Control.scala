package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/16      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 **  **
 */

import spinal.core._
import spinal.lib._
import Until._

case class SystolicArrayControl() extends Bundle {
  val load = Bool()
  val zeroes = Bool()
}

/* add size bundle in the array */
class SystolicArrayControlWithSize(val depth:Long) extends SystolicArrayControl with Size{
  override val size: UInt = UInt(log2Up(depth) bits)
}


