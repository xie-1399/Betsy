package Betsy


/**
 * * Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 * * Update Time : 2024/6/6      SpinalHDL Version: 1.94      **
 * * You should have received a copy of the MIT License along with this library **
 * * **
 */


import spinal.core._
import spinal.lib._
import Betsy._
import BetsyLibs._
import Betsy.Instruction._
import Betsy.Until.{BetsyModule, zero}

/** Decode test order : NoOp -> DataMove -> LoadWeight -> MatMul -> Configure -> SIMD
 * this version instruction only enqueue when fire (so the instruction is blocked when before instruction not done !!!)
 * */

class Decode(arch: Architecture, Sampler: Boolean = false)(implicit layOut: InstructionLayOut) extends BetsyModule {

  val io = new Bundle {
    val instruction = slave Stream InstructionFormat(layOut.instructionSizeBytes * 8)
    val dram0 = master Stream MemControl(arch.dram0Depth)
    val dram1 = master Stream MemControl(arch.dram0Depth)
    val hostDataFlow = master Stream new HostDataFlowControl()
    val memPortA = master Stream MemControl(arch.localDepth)
    val memPortB = master Stream MemControl(arch.localDepth)
    val localDataFlow = master Stream new LocalDataFlowControlWithSize(arch.localDepth)
    val systolicArrayControl = master Stream SystolicArrayControl()
    val error = out Bool()
    val nop = out Bool()
    /* the noOP instruction */
    val pc = out(UInt(arch.pcWidth bits))
  }

  def setDefault[T <: Data](stream: Stream[T]): Unit = {
    stream.valid := False
    stream.payload.clearAll()
  }

  val pc = Reg(UInt(arch.pcWidth bits)).init(0)
  when(io.instruction.fire) {
    pc := pc + layOut.instructionSizeBytes /* 8 or 4 bytes better */
  }

  val runCycles = Reg(UInt(64 bits)).init(0)
  when(io.instruction.valid) {
    runCycles := runCycles + 1
  }

  val opcode = io.instruction.opcode
  val flags = io.instruction.flags
  val arguments = io.instruction.arguments
  val op0 = arguments(layOut.operand0SizeBits - 1 downto 0)
  val op1 = arguments(layOut.operand0SizeBits + layOut.operand1SizeBits - 1 downto layOut.operand0SizeBits)
  val op2 = arguments(layOut.operandsSizeBits - 1 downto layOut.operand0SizeBits + layOut.operand1SizeBits)

  // the systolic array control with size
  val systolicArrayControlHandler = new SizeHandler(new SystolicArrayControlWithSize(arch.localDepth), SystolicArrayControl(), arch.localDepth)
  systolicArrayControlHandler.io.output >> io.systolicArrayControl
  setDefault(systolicArrayControlHandler.io.into)

  // control the portA
  val PortAstrideHandler = new StrideHandler(new MemControlWithStride(arch.localDepth, arch.stride0Depth), MemControl(arch.localDepth), arch.localDepth)
  PortAstrideHandler.io.output >> io.memPortA
  setDefault(PortAstrideHandler.io.into)


  val Block = new Composite(this, "NoOp") {
    io.dram0.valid.clear()
    io.dram0.payload.clearAll()
    io.dram1.valid.clear()
    io.dram1.payload.clearAll()
    // io.memPortA.valid.clear()
    // io.memPortA.payload.clearAll()
    io.memPortB.valid.clear()
    io.memPortB.payload.clearAll()
    io.localDataFlow.valid.clear()
    io.localDataFlow.payload.clearAll()
    io.hostDataFlow.valid.clear()
    io.hostDataFlow.payload.clearAll()
    io.instruction.ready := False

    // build the nop instruction
    val isNoOp = io.instruction.valid && io.instruction.opcode === Opcode.NoOp
    when(isNoOp) {
      io.nop := True
      io.instruction.ready := True
    }.otherwise {
      io.nop := False
    }
  }

  //build the data move instruction path
  val dataMove = new Composite(this, "DataMove") {
    //      val isDataMove = io.instruction.valid && io.instruction.opcode === Opcode.DataMove
    //      val dataMoveArgs = DataMoveArgs.fromBits(op0,op1,op2)
    //      val dataMoveFlags = DataMoveFlags()
    //      dataMoveFlags.kind := flags.asUInt
    //
    //      val DataMoveQueueControl = new MultiEnqControl(1)
    //      DataMoveQueueControl.block()
    //      val dataMoveValidError = RegInit(False).setWhen(!DataMoveKind.isValid(dataMoveFlags.kind) && isDataMove)
    //      val dram0Gen = new MemControlWithStride(arch.dram0Depth,arch.stride1Depth)
    //      val dram1Gen = new MemControlWithStride(arch.dram1Depth,arch.stride1Depth)
    //
    //      val hostDataFlowHandler = new SizeHandler(cloneOf(new HostDataFlowControlWithSize(arch.localDepth)),cloneOf(new HostDataFlowControl()),arch.localDepth)
    //      hostDataFlowHandler.io.output >> io.hostDataFlow
    //
    //      import DataMoveKind._
    //      when(isDataMove){
    //        /* using the flags show */
    //        switch(flags.asUInt){
    //          is(dram0ToMemory){ //from the DRAM0 to the memory
    //            hostDataFlowHandler.io.into.valid := True
    //            hostDataFlowHandler.io.into.payload.size := dataMoveArgs.size.resized
    //            hostDataFlowHandler.io.into.payload.kind := HostDataFlowControl.In0
    //
    //          }
    //          is(memoryToDram0){
    //
    //          }
    //          is(dram1ToMemory){
    //
    //          }
    //
    //          is(memoryToDram1){
    //
    //          }
    //          is(accumulatorToMemory){
    //
    //          }
    //          is(memoryToAccumulator){
    //
    //          }
    //          is(memoryToAccumulatorAccumulate){
    //
    //          }
    //
    //        }
    //
    //      }
  }


  val loadWeight = new Composite(this, "LoadWeight") {
    /* load weight + load zeroes + noop  -> the weight will be loaded into the systolic array */
    /* simple instruction for just load the weight to the Array */
    val isLoad = io.instruction.valid && io.instruction.opcode === Opcode.LoadWeights
    val loadArgs = LoadWeightArgs.fromBits(op0, op1)
    val zeroes = flags(0)
    val loadError = isLoad && !LoadWeightFlags.isValid(flags)

    when(isLoad){
      /* load control signals */
      PortAstrideHandler.io.into.valid := isLoad && !zeroes
      PortAstrideHandler.io.into.payload.stride := loadArgs.stride
      PortAstrideHandler.io.into.payload.address := loadArgs.address
      PortAstrideHandler.io.into.payload.size := loadArgs.size.resized
      PortAstrideHandler.io.into.payload.write := False

      systolicArrayControlHandler.io.into.valid := isLoad
      systolicArrayControlHandler.io.into.zeroes := zeroes
      systolicArrayControlHandler.io.into.load := isLoad
      systolicArrayControlHandler.io.into.size := (loadArgs.size - 1).resized /* check the weight load number*/
    }

    when(io.instruction.opcode === Opcode.LoadWeights) {
      io.localDataFlow.valid := isLoad && !zeroes
      io.localDataFlow.payload.size := loadArgs.size.resized
      io.localDataFlow.payload.sel := LocalDataFlowControl.memoryToArrayWeight
      when(zeroes) {
        io.instruction.ready := systolicArrayControlHandler.io.into.ready
      }.otherwise {
        io.instruction.ready := PortAstrideHandler.io.into.ready
      }
    }
  }

  val matMul = new Composite(this, "MatMul") {
    /* if zero will run zero and let data move array -> accumulator
    ** if accumulate will output and Memory -> Array -> Acc*/
    val ismatMul = io.instruction.valid && io.instruction.opcode === Opcode.MatMul
    val matMulArgs = MatMulArgs.fromBits(op0,op1,op2)
    val zeroes = flags(0)
    val accumulate = flags(1)
    val matMulError = ismatMul && !MatMulFlags.isValid(flags.asUInt)

  }

  val configure = new Composite(this, "Configure") {

    import Configure._

    /* the configure instruction is used to configure some regs in the NPU */
    val isConfigure = io.instruction.valid && io.instruction.opcode === Opcode.Configure
    val configureArgs = ConfigureArgs(op1, op0)
    val configureError = !isValid(op0.asUInt)

    when(isConfigure) {
      switch(configureArgs.register.asUInt) {
        is(programCounter) {
          pc := configureArgs.value.asUInt.resized
          io.instruction.ready := True
        }
        is(runningCycles) {
          runCycles := configureArgs.value.asUInt.resized
          io.instruction.ready := True
        }
        // Todo add more registers
      }
    }
  }

  val SIMD = new Composite(this, "SIMD") {

  }

  val HasError = RegInit(False).setWhen(
    io.instruction.valid && Opcode.Operror(io.instruction.opcode) || loadWeight.loadError || configure.configureError || matMul.matMulError
  )

  io.error := HasError
  io.pc := pc
}

object Decode extends App {
  val arch = Architecture.tiny()
  val layOut = InstructionLayOut(arch)
  SpinalSystemVerilog(new Decode(arch)(layOut))
}