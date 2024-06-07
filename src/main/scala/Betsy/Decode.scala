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
import BetsyLibs._
import Betsy.Instruction.{DataMoveArgs, DataMoveFlags, DataMoveKind}
import Betsy.Until.BetsyModule

  /**
  Decode test order : NoOp -> DataMove -> LoadWeight -> MatMul -> Configure -> SIMD

  this version instruction only enqueue when fire (so the instruction is blocked when before instruction not done !!!)

   **/

class Decode(arch:Architecture,pipeline:Boolean = true)(implicit layOut: InstructionLayOut) extends BetsyModule{

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
    val op0 = arguments(layOut.operand0SizeBits - 1 downto 0)
    val op1 = arguments(layOut.operand0SizeBits + layOut.operand1SizeBits - 1 downto layOut.operand0SizeBits)
    val op2 = arguments(layOut.operandsSizeBits - 1 downto layOut.operand0SizeBits + layOut.operand1SizeBits)

    val Block = new Composite(this,"NoOp"){
      io.dram0.valid.clear()
      io.dram0.payload.clearAll()
      io.dram1.valid.clear()
      io.dram1.payloadclearAll()
      io.hostDataFlow.valid.clear()
      io.hostDataFlow.payload.clearAll()
      io.memPortA.valid.clear()
      io.memPortA.payload.clearAll()
      io.memPortB.valid.clear()
      io.memPortB.payload.clearAll()
      io.localDataFlow.valid.clear()
      io.localDataFlow.payload.clearAll()
      io.instruction.ready := False

      val isNoOp = io.instruction.valid && io.instruction.opcode === Opcode.NoOp
      io.instruction.ready.setWhen(isNoOp)
    }


    val DataMove = new Composite(this,"DataMove"){
      val isDataMove = io.instruction.valid && io.instruction.opcode === Opcode.DataMove
      val dataMoveArgs = DataMoveArgs.fromBits(op0,op1,op2)
      val dataMoveFlags = DataMoveFlags()
      dataMoveFlags.kind := flags.asUInt

      val DataMoveQueueControl = new MultiEnqControl(1)
      DataMoveQueueControl.block()
      val dataMoveValidError = RegInit(False).setWhen(!DataMoveKind.isValid(dataMoveFlags.kind) && isDataMove)
      import DataMoveKind._
      when(isDataMove){
        /* using the flags show */
        switch(flags){
          is(dram0ToMemory){ //from the DRAM0 to the memory
            // val d02mStrideHandler = new StrideHandler()
            // DataMoveQueueControl.enqueue(io.instruction.valid,io.dram0,)
          }

        }

      }
    }

    val LoadWeight = new Composite(this,"LoadWeight"){

    }

    val MatMul = new Composite(this,"MatMul"){

    }

    val Configure = new Composite(this,"Configure"){

    }

    val SIMD = new Composite(this,"SIMD"){

    }

  val HasError = RegInit(False).setWhen(
    (io.instruction.valid && Opcode.Operror(io.instruction.opcode)) ||
      DataMove.dataMoveValidError
  )
    io.error := HasError
}

object Decode extends App{
  val arch = Architecture.tiny()
  val layOut = InstructionLayOut(arch)
  SpinalSystemVerilog(new Decode(arch)(layOut))
}