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

case class MemControl(depth:Int) extends Bundle with Size{
  val write = Bool()
  val address = UInt(log2Up(depth) bits)
  val size = UInt(log2Up(depth) bits)
}

case class Port[T <: Data](gen:HardType[T], depth:Int) extends Bundle with IMasterSlave {
  val control = Stream(MemControl(depth))
  val dataIn = Stream(gen)
  val dataOut = Stream(gen)

  override def asMaster(): Unit = {
    master(control,dataIn)
    slave(dataOut)
  }
}

class DualPortMem[T <: Data](gen:HardType[T], depth:Int
                             ,name:String = "") extends BetsyModule{

  val io = new Bundle{
    val portA = slave(Port(gen,depth = depth))
    val portB = slave(Port(gen,depth = depth))
  }

  def connectPorts(port: Port[T],innerport: InnerPort[T]) = {

    innerport.address := port.control.address
    when(port.control.write){

    }

  }

  val memoryImpl = new MemoryImpl(gen,depth,2,BlockRAM)  /* 2 ports block ram */

  /* connect the ports */


}
