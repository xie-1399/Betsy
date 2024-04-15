package Betsy.Experimental

import Betsy.Until._
import spinal.core._

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */


class TPUArray[T <: Data with Num[T]](gen:HardType[T],height:Int = 3,width:Int = 3) extends BetsyModule{
  val io = new Bundle{
    val weight = in Vec(gen,height)
    val activation = in Vec(gen,width)
  }

  val PEs = for(i <- 0 until height)
    yield for(j <- 0 until width) yield {new simpleMac(gen)}

  /* connect the PEs with TPU-like */



}
