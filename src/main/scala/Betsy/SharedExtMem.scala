package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

import spinal.core._
import spinal.lib._
import Betsy.Until._

case class ExtMemIO[T <: Data](dataType:HardType[T]) extends Bundle with IMasterSlave {
  val ren = Bool()
  val raddr = UInt()
  val rdata = dataType()

  val wen = Bool()
  val waddr = UInt()
  val wdata = dataType()

  override def asMaster(): Unit = {

  }
}

class SharedExtMem extends BetsyModule {

  val io = new Bundle{

  }
}
