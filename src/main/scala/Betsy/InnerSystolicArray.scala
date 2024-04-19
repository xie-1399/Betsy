package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94     **
 ** You should have received a copy of the MIT License along with this library **
 ** The  InnerSystolicArray is a WS data flow drive Array    **
 ** The performance of the Array is calculate ([height * width ] * [width × height])
 ** load cost width cycles , calculate cost height + width + height cycles **
 ** Test Status : PASS :)         Version:0.1 **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib.Delay

class InnerSystolicArray[T <: Data with Num[T]](gen:HardType[T],height:Int,width:Int) extends BetsyModule{
  val io = new Bundle{
    val load = in Bool()
    val weight = in(Vec(gen,height))
    val input = in(Vec(gen,width))
    val output = out(Vec(gen,height))
  }

  val mac = for(i <- 0 until height) yield for(j <- 0 until width) yield {
    new MAC(gen,name = s"_${i}_${j}_")
  }

  val bias = Vec(Reg(gen).init(zero(gen.craft())),height)
  when(io.load){
    bias := io.weight
  }

  /* connect the first row */
  for(j <- 0 until width){
    mac(0)(j).io.load := io.load
    mac(0)(j).io.mulInput := Delay(io.input(j),j,init = zero(gen()))
    if(j > 0){
      mac(0)(j).io.addInput := mac(0)(j - 1).io.macOut
    }
  }

  /* connect the first colum */
  for(i <- 0 until height){
    mac(i)(0).io.addInput := bias(i)
    if(i > 0){
      mac(i)(0).io.load := io.load /* let not assign again with the first row */
      mac(i)(0).io.mulInput := mac(i - 1)(0).io.passthrough
    }
  }

  /* connect the body */
  for( i <- 1 until height){
    for( j <- 1 until width){
      mac(i)(j).io.load := io.load
      mac(i)(j).io.mulInput := mac(i - 1)(j).io.passthrough
      mac(i)(j).io.addInput := mac(i)(j - 1).io.macOut
    }
  }

  /* let all outputs can be calculate once */
  for(i <- 0 until height){
    io.output(i) := Delay(mac(i)(width - 1).io.macOut,height - (i + 1),init = zero(gen()))
  }
}


object InnerSystolicArray extends App{
  /* the test is about the weight * matrix and matrix * matrix */
  SpinalSystemVerilog(new InnerSystolicArray(UInt(16 bits),16,16))
}
