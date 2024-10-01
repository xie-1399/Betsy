package Betsy.Softmax

import Betsy.Until._
import Betsy.Operations._
import spinal.core._
import spinal.lib._
import BetsyLibs.{MultiVecAdder, SIMCFG}

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/9/25      SpinalHDL Version: 1.94       **
 ** calculate the Exponential by the taylor series **
 ** ~ 4 cycles without pipeline (4 mac operations) ~
 */

class Exponential[T <: Data with Num[T]](gen:HardType[T],len:Int) extends BetsyModule{

  require(gen.craft().isInstanceOf[AFix], "the fixed point value is needed")

  val io = new Bundle{
    val inputs = slave Stream Vec(gen(),len)
    val outputs = master Stream Vec(gen(),len)
  }

  // one cycle calculate and one cycle accumulate
  val part_regs = Vec(Vec(Reg(gen()).init(constConvert(gen(),0)),len),4)  // each part sum reg file

  part_regs(0).foreach{
    r =>
      // r := mac()
  }


//  val exp_regs = Vec(Reg(gen()).init(constConvert(gen(),0)),len)
//  val multiVecAdder = new MultiVecAdder(gen,size = len,vecNum = n_series)
//  multiVecAdder.io.inputs.payload := part_regs
//  multiVecAdder.io.inputs.valid := RegNext(io.inputs.valid).init(False)
//
//  when(io.inputs.valid){
//    // each part + 1 and no careful about the divider
//    // n_serials == 1
//    part_regs(0).foreach(_ := constConvert(gen(),1))
//    part_regs(1) := io.inputs.payload
//    if(n_series == 3){ // square operation
//      part_regs(2).assignFromBits(io.inputs.payload.map(v => square(v)).map(v => v >> 1).asBits())
//
//    }
//    else if(n_series == 4){
//      part_regs(2).assignFromBits(io.inputs.payload.map(v => square(v)).map(v => v >> 1).asBits())
//      part_regs(3).assignFromBits(io.inputs.payload.map(v => cube(v)).map(v => v/constConvert(gen(),6)).asBits())
//    }
//  }
//  io.outputs << multiVecAdder.io.outputs
//  io.inputs.ready := multiVecAdder.io.outputs.valid && multiVecAdder.io.inputs.ready // fire
}

//object Exponential extends App{
//  import spinal.core.sim._
//  SIMCFG().compile{
//    val dut = new Exponential(AFix(7 exp, -8 exp, true),4,4)
//    dut
//  }.doSimUntilVoid{
//    dut =>
//      dut.clockDomain.forkStimulus(10)
//
//      dut.io.inputs.valid #= true
//      dut.io.outputs.ready #= true
//      dut.io.inputs.payload(0) #= 1.25
//      dut.io.inputs.payload(1) #= 2.25
//      dut.io.inputs.payload(2) #= -2.75
//      dut.io.inputs.payload(3) #= 3.5
//      dut.clockDomain.waitSamplingWhere(dut.io.outputs.valid.toBoolean)
//      println(dut.io.outputs.payload(0).toDouble)
//      println(dut.io.outputs.payload(1).toDouble)
//      println(dut.io.outputs.payload(2).toDouble)
//      println(dut.io.outputs.payload(3).toDouble)
//      simSuccess()
//  }
//}