package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/14      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** MAC Unit in the Accelerator
 ** Test Status : PASS :)         Version:0.1 **
 */

import spinal.core._
import Betsy.Until._
import Operations._

class MAC[T<: Data with Num[T]](gen:HardType[T],name:String = "") extends BetsyModule{

  val io = new Bundle{
    val load =  in Bool()
    val mulInput = in(gen())
    val addInput = in(gen())

    val passthrough = out(gen())
    val macOut = out(gen)
  }

  val weight = Reg(gen()).init(constConvert(gen(),0)).setName(name + "inner_weight")
  val macOut = Reg(gen()).init(constConvert(gen(),0)).setName(name + "inner_macOut")

  io.passthrough := RegNext(io.mulInput).init(constConvert(gen(),0))

  when(io.load){ /* load the weight*/
    weight := io.addInput
    io.macOut := weight
  }.otherwise{
    val macTemp = mac(io.mulInput,weight,io.addInput)
    /* the mac out unit should be resized to adapt the bit width */
    macOut := macTemp
    io.macOut := macOut
  }
}