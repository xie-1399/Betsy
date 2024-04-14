package Sloan

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** MAC Unit in the Accelerator **
 */

import Sloan.Until._
import spinal.core._
import spinal.lib._

class MAC[T<: Data with Num[T]](val gen:HardType[T]) extends SloanModule{

  val io = new Bundle{
    val load =  in Bool()
    val mulInput = in(gen())
    val addInput = in(gen())

    val passthrough = out(gen())
    val macOut = out(gen)
  }

  val weight = Reg(gen()).init(zero(gen.craft()))
  val macOut = Reg(gen()).init(zero(gen.craft()))

  io.passthrough := RegNext(io.mulInput).init(zero(gen.craft()))

  when(io.load){ /* load the weight*/
    weight := io.addInput
    io.macOut := weight
  }.otherwise{
    macOut := mac(gen.craft(),io.mulInput,weight,io.addInput).resized
    io.macOut := macOut
  }
}


object MAC extends App{
  SpinalSystemVerilog(new MAC(UInt(3 bits)))
}