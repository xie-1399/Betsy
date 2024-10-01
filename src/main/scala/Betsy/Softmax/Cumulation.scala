package Betsy.Softmax

import Betsy.Until._
import spinal.lib._
import spinal.core._
import Betsy.Operations._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/10/1      SpinalHDL Version: 1.94       **
 ** cumulate for the exponential sum -> support about the fixed point **
 */

//class Cumulation[T <: Data with Num[T]](gen:HardType[T],len:Int,vecNum:Int) extends BetsyModule{
//
//  val io = new Bundle{
//    val inputs = in(Vec(gen(),n_series))
//    val output = out(Vec(gen()))
//  }
//
//  val output = Reg(gen()).init(zero(gen()))
//
//  if(n_series == 2){
//    output := add(io.inputs(0),io.inputs(1))
//  }
//  else if(n_series == 3){
//    output := add(io.inputs(0),io.inputs(1))
//  }
//  else{
//
//  }
//
//  io.output := io.inputs.reduceBalancedTree(add())
//
//}
