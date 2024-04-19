package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94     **
 ** You should have received a copy of the MIT License along with this library **
 ** Wrapper the InnerSystolicArray as a SystolicArray with control **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._


class SystolicArray[T <: Data with Num[T]](gen:HardType[T],height:Int,width:Int,WhiteBox:Boolean = false) extends BetsyModule{

  val io = new Bundle{
    val control = slave Stream(SystolicArrayControl())
    val weight = slave Stream(Vec(gen, height))
    val input = slave Stream(Vec(gen, width))

    val output = master Stream(Vec(gen, height))
  }

  val array = new InnerSystolicArray(gen,height,width)



}
