package BetsyLibs

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/23      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the Vec Until contains some component about the vec operation **
 ** Test Status : PASS :)         Version:0.1                 **
 */

import Betsy.Until._
import Betsy.Operations._
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
      p._1 := add(io.left.payload(p._2),io.right.payload(p._2))
  }

  io.output.valid := io.left.valid && io.right.valid
  io.left.ready := io.output.ready && io.right.valid
  io.right.ready := io.output.ready && io.left.valid
}



// more than 3 vectors add operation
class MultiVecAdder[T <: Data with Num[T]](gen:HardType[T],size:Int,vecNum:Int) extends BetsyModule{
  val io = new Bundle{
    val inputs = slave Stream Vec(Vec(gen(),size),vecNum)
    val outputs = master Stream Vec(gen(),size)
  }
  // combine logic for the plus
  def plus(m1: Vec[T], m2: Vec[T]): Vec[T] = {
    val res = cloneOf(m1)
    res.zipWithIndex.foreach {
      r =>
        r._1 := add(m1(r._2), m2(r._2))
    }
    res
  }
  // one cycle later
  val inter_reg = Vec(Reg(gen()).init(zero(gen())),size)
  inter_reg := io.inputs.payload.reduceBalancedTree((op1, op2) => plus(op1, op2))

  io.outputs.valid := RegNext(io.inputs.valid).init(False)
  io.inputs.ready := io.outputs.ready
  io.outputs.payload := inter_reg
}
