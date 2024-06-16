package Betsy.Experimental

import Betsy.Until._
import spinal.core._
/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** TPU experimental Array with data control **
 ** The TPU Dataflow material is on : https://zhuanlan.zhihu.com/p/26522315 **
 */


class TPUArray[T <: Data with Num[T]](gen:HardType[T],accType:HardType[T],height:Int = 3,width:Int = 3) extends BetsyModule{
  val io = new Bundle{
    val weight = in Vec(gen,height)
    val activation = in Vec(gen,width)
    val accSum = out Vec(accType(),height * width)
  }

   val PEs = for(i <- 0 until height)
     yield for(j <- 0 until width) yield {new TPUMac(gen(),accType())}

  /* connect the PEs with TPU-like */
  /* first row*/
  PEs(0)(0).io.m1 := io.weight(0)
  PEs(0)(0).io.m2 := io.activation(0)

  for(j <- 1 until width){
    PEs(0)(j).io.m2 := io.activation(j)
    PEs(0)(j).io.m1 := PEs(0)(j-1).io.m1pass
  }

  /* first column */
  for(i <- 1 until height){
    PEs(i)(0).io.m1 := io.weight(i)
    PEs(i)(0).io.m2 := PEs(i - 1)(0).io.m2pass
  }

  /* connect the body like */
  for( i <- 1 until height){
    for( j <- 1 until width){
      PEs(i)(j).io.m1 := PEs(i)(j-1).io.m1pass
      PEs(i)(j).io.m2 := PEs(i-1)(j).io.m2pass
    }
  }

  val sums = PEs.flatten.map(_.io.psum)
  io.accSum.zipWithIndex.map{
    acc =>
      acc._1 := sums(acc._2)
  }
}