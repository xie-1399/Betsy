package Betsy.common

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the common mac unit support the SInt and UInt Mac operation **
 */

import spinal.core._
import spinal.lib._
import Betsy.Until._

trait betsyData{}
object float extends betsyData
object uint extends betsyData
object sint extends betsyData

class MacUnit[T<:Data](dataType:betsyData = sint,dataWidth:Int) extends BetsyModule {

  val inType = dataType match {
    case `sint` => SInt(dataWidth bits)
    case _ => UInt(dataWidth bits)
  }
  val outType = dataType match {
    case `sint` => SInt(2 * dataWidth bits)
    case _ => UInt(2 * dataWidth bits)
  }
  val io = new Bundle{
    val m1 = in(inType)
    val m2 = in(inType)
    val acc = in(inType)
    val macOut = out(outType)
  }

  if(dataType == sint){
    io.macOut := io.m1.asInstanceOf[SInt] * io.m2.asInstanceOf[SInt] + io.acc.asInstanceOf[SInt]
  }
  else if(dataType == uint){
    io.macOut := io.m1.asInstanceOf[UInt] * io.m2.asInstanceOf[UInt] + io.acc.asInstanceOf[UInt]
  }
}

object MacUnit extends App{
  SpinalSystemVerilog(new MacUnit(sint,3))
}