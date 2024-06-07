package Betsy


/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/6/6      SpinalHDL Version: 1.94      **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */


import spinal.core._
import spinal.lib._
import Betsy._
import Betsy.Until.BetsyModule

  /**
  Decode test order : NoOp -> DataMove -> LoadWeight -> MatMul -> Configure -> SIMD

  this version instruction only enqueue when fire (so the instruction is blocked when before instruction not done !!!)

   **/

class Decode(arch:Architecture)(implicit layOut: InstructionLayOut) extends BetsyModule{

  val io = new Bundle{
    val instruction = slave Stream InstructionFormat(layOut.instructionSizeBytes * 8)
    val dram0 = master Stream MemControl(arch.dram0Depth)
    val dram1 = master Stream MemControl(arch.dram0Depth)
    val hostDataFlow = master Stream new HostDataFlowControl()
    val memPortA = master Stream MemControl(arch.localDepth)
    val memPortB = master Stream MemControl(arch.localDepth)
    val localDataFlow = master Stream new LocalDataFlowControlWithSize(arch.localDepth)

    val error = out Bool()
  }

    val opcode = io.instruction.opcode
    val flags = io.instruction.flags
    val arguments = io.instruction.arguments
    val OpHasError = io.instruction.valid && Opcode.Operror(io.instruction.opcode)


    val DataMove = new Composite(this,"DataMove"){
      val isDataMove = io.instruction.valid && io.instruction.opcode === Opcode.DataMove
      when(isDataMove){
        /* using the flags show */

      }
    }

  io.error := OpHasError



}
