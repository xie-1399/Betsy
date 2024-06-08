package Betsy

import spinal.core._
import spinal.lib._
import BetsyLibs._
import Betsy.Until._
/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/31      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** integration the compute/control/datamove module in the BetsyNPU**
 */

/* the npu top temporal */

class Top[T <: Data with Num[T]](gen:HardType[T],arch: Architecture,log:Boolean = false) extends BetsyModule{

  val instructionLayOut = InstructionLayOut(arch,gen = log)
  require(gen.getBitsWidth == arch.dataWidth,"the clare data width is not match in the arch !!! ")
  require(gen().getClass.toString.split("\\.").last == arch.dataKind , "the clare data type is not match in the arch !!!")

  val io = new Bundle{
    val dram0 = master(BetsyStreamPass(gen))
    val dram1 = master(BetsyStreamPass(gen))
  }


  // val localRouter = new LocalRouter(gen,arch)
  val hostRouter = new HostRouter(gen)
  val scratchPad = new DualPortMem(gen,arch.localDepth) // no mask with
  scratchPad.io.portA.blockPort()

  hostRouter.io.mem.dataIn << scratchPad.io.portB.dataOut
  hostRouter.io.mem.dataOut >> scratchPad.io.portB.dataIn

  hostRouter.io.dram1 <> io.dram1
  hostRouter.io.dram0 <> io.dram0
}

object Top extends App{
  SpinalSystemVerilog(new Top(SInt(4 bits),Architecture.tiny()))
}