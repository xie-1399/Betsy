package BetsyLibs

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/23      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the Vec Until contains some component about the vec operation **
 ** Test Status : PASS :)         Version:0.1                 **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._

/* just two vectors add with stream control*/
class VecAdder[T <: Data with Num[T]](gen:HardType[T],size:Int) extends BetsyModule{

  val io = new Bundle{
    val left = slave(Stream(Vec(gen(),size)))
    val right = slave(Stream(Vec(gen(),size)))
    val output = master(Stream(Vec(gen(),size)))
  }

  /* with the clip down value */
  io.output.payload.zipWithIndex.foreach{
    p =>
      p._1 := upDown(io.left.payload(p._2) +^ io.right.payload(p._2),gen.craft()).resized
  }

  io.output.valid := io.left.valid && io.right.valid
  io.left.ready := io.output.ready && io.right.valid
  io.right.ready := io.output.ready && io.left.valid
}