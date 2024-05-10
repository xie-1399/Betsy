package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

import Betsy.Until.BetsyModule
import BetsyLibs.BetsyFIFO
import spinal.core._
import spinal.lib._

// Todo with the Alu logic

class ALUArray[T <: Data with Num[T]](gen:HardType[T],arch: Architecture) extends BetsyModule{

  val layOut = InstructionLayOut(arch)

  val io = new Bundle{
    val instruction = slave(Stream(ALUInstruction(layOut)))
    val inputs = slave(Stream(Vec(gen,arch.arraySize)))
    val outputs = master(Stream(Vec(gen,arch.arraySize)))
  }
  // val outputsFifo = StreamFifo(cloneOf(io.outputs.payload),2)  /* pipeline control with a stream fifo (2 stages)*/
  val outputsFifo = new BetsyFIFO(cloneOf(io.outputs.payload),2)  /* using the BetsyFIFO */

  /* connect the Alu array with io */
  val alus = for(idx <- 0 until arch.arraySize) yield {
    val alu = new ALU(gen,numOps = ALUOp.numOps,numRegisters = arch.simdRegistersDepth,inputPipe = true,outputPipe = true)
    alu.io.op := Mux(io.instruction.fire,io.instruction.payload.op,U(ALUOp.NoOp).resized)
      // io.instruction.payload.op
    alu.io.input := io.inputs.payload(idx)
    alu.io.sourceLeft := io.instruction.payload.sourceLeft
    alu.io.sourceRight := io.instruction.payload.sourceRight
    alu.io.dest := io.instruction.payload.dest
    outputsFifo.io.push.payload(idx) := alu.io.output
  }
  io.outputs <> outputsFifo.io.pop

  val inputNotNeeded = io.instruction.op === ALUOp.NoOp || io.instruction.op === ALUOp.Zero ||
    (ALUOp.isUnary(io.instruction.op) && io.instruction.sourceLeft =/= 0) || (io.instruction.sourceLeft =/= 0 && io.instruction.sourceRight =/= 0)
  val inputNeeded = !inputNotNeeded

  /* Notice it's reset false */
  outputsFifo.io.push.valid := Delay(io.instruction.valid && (io.inputs.valid || inputNotNeeded),2,init = False) // delay 2 cycles for the fifo and set it as False
  io.instruction.ready := ((io.inputs.valid || inputNotNeeded) && io.outputs.ready)
  io.inputs.ready := io.outputs.ready && io.instruction.valid && inputNeeded
}

object ALUArray extends App{
  SpinalSystemVerilog(new ALUArray(UInt(8 bits),Architecture())) /* 16 * ALU Array */
}