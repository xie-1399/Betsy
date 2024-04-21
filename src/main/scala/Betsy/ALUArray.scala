package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/21      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

import Betsy.Until.BetsyModule
import spinal.core._
import spinal.lib._

class ALUArray[T <: Data with Num[T]](gen:HardType[T],arch: Architecture) extends BetsyModule{

  val layOut = InstructionLayOut(arch)

  val io = new Bundle{
    val instruction = slave(Stream(ALUInstruction(layOut)))
    val inputs = slave(Stream(Vec(gen,arch.arraySize)))
    val outputs = master(Stream(Vec(gen,arch.arraySize)))
  }

  /* connect the Alu array with io*/
  val alus = for(idx <- 0 until arch.arraySize) yield {
    val alu = new ALU(gen,numOps = ALUOp.numOps,numRegisters = arch.simdRegistersDepth,inputPipe = true,outputPipe = true)
    alu.io.op := Mux(io.instruction.fire,io.instruction.payload.op,U(ALUOp.NoOp).resized)
    alu.io.input := io.inputs.payload(idx)
    alu.io.sourceLeft := io.instruction.payload.sourceLeft
    alu.io.sourceRight := io.instruction.payload.sourceRight
    alu.io.dest := io.instruction.payload.dest
    io.outputs.payload(idx) := alu.io.output
  }

  /* pipeline control with a stream fifo */
  val outputsFifo = StreamFifo(io.outputs.payload,2)
  io.outputs <> outputsFifo.io.pop

  /* Todo ALU logic ... */
}
