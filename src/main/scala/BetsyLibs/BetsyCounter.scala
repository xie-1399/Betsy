package BetsyLibs


/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/14      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the simple Counter used for the Betsy Module **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._

class BetsyCounter(number:Long) extends BetsyModule {
  val io = new Bundle{
    val value = master Stream UInt(log2Up(number) bits)
    val resetValue = in Bool()
  }
  val value = Reg(UInt(log2Up(number) bits)).init(0)
  when(io.value.fire){
    when(value === U(number - 1)){
      value.clearAll()
    }.otherwise{
      value := value + 1
    }
  }
  when(io.resetValue){
    value.clearAll()
  }
  io.value.valid := !io.resetValue
  io.value.payload := value
}

object BetsyCounter{
  def apply(number: Long): BetsyCounter = {
    val counter = new BetsyCounter(number)
    counter.io.value.ready := False
    counter.io.resetValue := False
    counter
  }
}