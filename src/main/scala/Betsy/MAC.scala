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

class MAC[T<: Data with Num[T]](gen:HardType[T],clip:Boolean = true,name:String = "") extends BetsyModule{

  val io = new Bundle{
    val load =  in Bool()
    val mulInput = in(gen())
    val addInput = in(gen())

    val passthrough = out(gen())
    val macOut = out(gen)
  }

  val weight = Reg(gen()).init(zero(gen.craft())).setName(name + "weight")
  val macOut = Reg(gen()).init(zero(gen.craft())).setName(name + "macOut")

  io.passthrough := RegNext(io.mulInput).init(zero(gen.craft()))

  when(io.load){ /* load the weight*/
    weight := io.addInput
    io.macOut := weight
  }.otherwise{
    val macTemp = if(clip) {
      upDown(mac(gen.craft(),io.mulInput,weight,io.addInput),gen()).resized /* clip the value */
    } else {
      mac(gen.craft(),io.mulInput,weight,io.addInput).resized    /* just overflow resize */
    }
    macOut := macTemp
    io.macOut := macOut
  }
}