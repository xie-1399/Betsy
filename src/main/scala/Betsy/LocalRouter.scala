package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94      **
 ** You should have received a copy of the MIT License along with this library **
 ** the local router control the data move between Accumulator/ Array / Scratch Pad**
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._

class LocalRouter[T <: Data](gen:HardType[T]) extends BetsyModule{

  val io = new Bundle{

  }


}
