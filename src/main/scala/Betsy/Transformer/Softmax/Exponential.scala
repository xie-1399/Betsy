//package Betsy.Transformer.Softmax
//
//import Betsy.Until._
//import Betsy.Operations._
//import spinal.core._
//import spinal.lib._
//
///**
// ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
// ** Update Time : 2024/9/25      SpinalHDL Version: 1.94       **
// ** calculate the Exponential by the taylor series **
// */
//
//
//class Exponential[T <: Data with Num[T]](gen:HardType[T],len:Int,n_series:Int = 4) extends BetsyModule{
//
//  require(gen.craft().isInstanceOf[UFix] || gen.craft().isInstanceOf[SFix], "the fixed point value is needed")
//  require(n_series >= 1 && n_series <=3, "illegal n_serial number in the Exponential module")
//
//  val io = new Bundle{
//    val inputs = slave Stream Vec(gen(),len)
//    val outputs = master Stream Vec(gen(),len)
//  }
//
//  // one cycle calculate and one cycle accumulate
//  val part_regs = Vec(Vec(Reg(gen()).init(zero(gen())),len),n_series)  // each part sum reg file
//  part_regs(0).foreach(_ := one(gen()))
//  part_regs(1) := io.inputs.payload
//
//
//  when(io.inputs.valid){
//    // each part + 1 and no careful about the divider
//    if(n_series == 2){ // square operation
//      part_regs(2).assignFromBits(io.inputs.payload.map(v => square(v)).map(v => v/2).asBits())
//    }
//    else if(n_series == 3){
//      part_regs(2).assignFromBits(io.inputs.payload.map(v => square(v)).asBits())
//      part_regs(3)
//    }
//  }
//
//  // cumulation
//
//}
