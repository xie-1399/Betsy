package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/19      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Dual Port memory unit
 ** **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._
import BetsyLibs._

case class MemControl(depth:Long) extends Bundle with Size{
  val write = Bool()
  val address = UInt(log2Up(depth) bits)
  val size = UInt(log2Up(depth) bits)
}

case class Port[T <: Data](gen:HardType[T], depth:Long) extends Bundle with IMasterSlave {
  val control = Stream(MemControl(depth))
  val dataIn = Stream(gen)
  val dataOut = Stream(gen)

  override def asMaster(): Unit = {
    master(control,dataIn)
    slave(dataOut)
  }
}




class DualPortMem[T <: Data](gen:HardType[T], depth:Long
                             ,name:String = "") extends BetsyModule{

  val io = new Bundle{
    // val portA =

  }
}
