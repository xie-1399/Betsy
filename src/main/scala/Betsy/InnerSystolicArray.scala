package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

import Betsy.Until._
import spinal.core._


class InnerSystolicArray[T <: Data with Num[T]](val gen:HardType[T],height:Int,width:Int) extends BetsyModule{

  val io = new Bundle{
    val load = in Bool()
    val weight = in(Vec(gen,height))
    val input = in(Vec(gen,width))
    val output = out(Vec(gen,height))
  }

  /* connect the first row */
  //for(j <- 0 )



}
