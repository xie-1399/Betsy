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
import Betsy.Until._

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
    val accumulatorWithALUArrayControl = master Stream AccumulatorWithALUArrayControl(layOut)
    val error = out Bool()
    val nop = out Bool() /* the noOP instruction */
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

  //control the portB
  val PortBstrideHandler = new StrideHandler(new MemControlWithStride(arch.localDepth, arch.stride1Depth), MemControl(arch.localDepth), arch.localDepth)
  PortBstrideHandler.io.output >> io.memPortB
  setDefault(PortBstrideHandler.io.into)

  // accumulator stride/size handler
  val accumulatorHandler = new StrideHandler(new AccumulatorMemControlWithSizeWithStride(layOut), AccumulatorMemControl(layOut), arch.accumulatorDepth)
  accumulatorHandler.io.output.payload.toAccumulatorWithALUArrayControl(arch) <> io.accumulatorWithALUArrayControl.payload
  io.accumulatorWithALUArrayControl.arbitrationFrom(accumulatorHandler.io.output)
  setDefault(accumulatorHandler.io.into)

  val dram0Handler = new StrideHandler(new MemControlWithStride(arch.dram0Depth, arch.stride0Depth), MemControl(arch.dram0Depth),arch.dram0Depth)
  dram0Handler.io.output >> io.dram0
  setDefault(dram0Handler.io.into)

  val dram1Handler = new StrideHandler(new MemControlWithStride(arch.dram1Depth, arch.stride1Depth), MemControl(arch.dram1Depth), arch.dram1Depth)
  dram1Handler.io.output >> io.dram1
  setDefault(dram1Handler.io.into)

  val hostDataFlowHandler = new SizeHandler(cloneOf(new HostDataFlowControlWithSize(arch.localDepth)), cloneOf(new HostDataFlowControl()), arch.localDepth)
  hostDataFlowHandler.io.output >> io.hostDataFlow
  setDefault(hostDataFlowHandler.io.into)

  val twoQueue = new MultiEnqControl(2)
  twoQueue.block()

  val threeQueue = new MultiEnqControl(3)
  threeQueue.block()

  val fourQueue = new MultiEnqControl(4)
  fourQueue.block()

  val Block = new Composite(this, "NoOp") {
    io.localDataFlow.valid.clear()
    io.localDataFlow.payload.clearAll()
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
    val isDataMove = io.instruction.valid && io.instruction.opcode === Opcode.DataMove
    val dataMoveArgs = DataMoveArgs.fromBits(op0, op1, op2)
    val dataMoveFlags = DataMoveFlags()
    dataMoveFlags.kind := flags.asUInt
    val dataMoveError = isDataMove && !DataMoveKind.isValid(flags.asUInt)
    val dataMoveValidError = RegInit(False).setWhen(!DataMoveKind.isValid(dataMoveFlags.kind) && isDataMove)

    import DataMoveKind._

    when(isDataMove) {
      /* using the flags show */
      switch(flags.asUInt) {
        is(dram0ToMemory) { //from the DRAM0 to the memory
          hostDataFlowHandler.io.into.valid := isDataMove
          hostDataFlowHandler.io.into.payload.size := dataMoveArgs.size.resized
          hostDataFlowHandler.io.into.payload.kind := HostDataFlowControl.In0

          PortBstrideHandler.io.into.valid := isDataMove
          PortBstrideHandler.io.into.payload.write := True
          PortBstrideHandler.io.into.payload.stride := dataMoveArgs.memStride
          PortBstrideHandler.io.into.payload.address := dataMoveArgs.memAddress
          PortBstrideHandler.io.into.payload.size := dataMoveArgs.size.resized

          dram0Handler.io.into.valid := isDataMove
          dram0Handler.io.into.payload.stride := dataMoveArgs.accStride
          dram0Handler.io.into.payload.address := dataMoveArgs.accAddress
          dram0Handler.io.into.payload.write := False
          dram0Handler.io.into.payload.size := dataMoveArgs.size.resized

          io.instruction.ready := threeQueue.Readyenqueue3(isDataMove,hostDataFlowHandler.io.into.ready,PortBstrideHandler.io.into.ready,dram0Handler.io.into.ready)
        }
        is(memoryToDram0) { // from memory to dram0
          hostDataFlowHandler.io.into.valid := isDataMove
          hostDataFlowHandler.io.into.payload.size := dataMoveArgs.size.resized
          hostDataFlowHandler.io.into.payload.kind := HostDataFlowControl.Out0

          PortBstrideHandler.io.into.valid := isDataMove
          PortBstrideHandler.io.into.payload.write := False
          PortBstrideHandler.io.into.payload.stride := dataMoveArgs.memStride
          PortBstrideHandler.io.into.payload.address := dataMoveArgs.memAddress
          PortBstrideHandler.io.into.payload.size := dataMoveArgs.size.resized

          dram0Handler.io.into.valid := isDataMove
          dram0Handler.io.into.payload.stride := dataMoveArgs.accStride
          dram0Handler.io.into.payload.address := dataMoveArgs.accAddress
          dram0Handler.io.into.payload.write := True
          dram0Handler.io.into.payload.size := dataMoveArgs.size.resized

          io.instruction.ready := threeQueue.Readyenqueue3(isDataMove,hostDataFlowHandler.io.into.ready,PortBstrideHandler.io.into.ready,dram0Handler.io.into.ready)

        }
        is(dram1ToMemory) { // from dram1 to the memory
          hostDataFlowHandler.io.into.valid := isDataMove
          hostDataFlowHandler.io.into.payload.size := dataMoveArgs.size.resized
          hostDataFlowHandler.io.into.payload.kind := HostDataFlowControl.In1

          PortBstrideHandler.io.into.valid := isDataMove
          PortBstrideHandler.io.into.payload.write := True
          PortBstrideHandler.io.into.payload.stride := dataMoveArgs.memStride
          PortBstrideHandler.io.into.payload.address := dataMoveArgs.memAddress
          PortBstrideHandler.io.into.payload.size := dataMoveArgs.size.resized

          dram1Handler.io.into.valid := isDataMove
          dram1Handler.io.into.payload.stride := dataMoveArgs.accStride
          dram1Handler.io.into.payload.address := dataMoveArgs.accAddress
          dram1Handler.io.into.payload.write := False
          dram1Handler.io.into.payload.size := dataMoveArgs.size.resized

          io.instruction.ready := threeQueue.Readyenqueue3(isDataMove, hostDataFlowHandler.io.into.ready, PortBstrideHandler.io.into.ready, dram1Handler.io.into.ready)
        }

        is(memoryToDram1) { // from memory to dram1

          hostDataFlowHandler.io.into.valid := isDataMove
          hostDataFlowHandler.io.into.payload.size := dataMoveArgs.size.resized
          hostDataFlowHandler.io.into.payload.kind := HostDataFlowControl.Out1

          PortBstrideHandler.io.into.valid := isDataMove
          PortBstrideHandler.io.into.payload.write := False
          PortBstrideHandler.io.into.payload.stride := dataMoveArgs.memStride
          PortBstrideHandler.io.into.payload.address := dataMoveArgs.memAddress
          PortBstrideHandler.io.into.payload.size := dataMoveArgs.size.resized

          dram1Handler.io.into.valid := isDataMove
          dram1Handler.io.into.payload.stride := dataMoveArgs.accStride
          dram1Handler.io.into.payload.address := dataMoveArgs.accAddress
          dram1Handler.io.into.payload.write := True
          dram1Handler.io.into.payload.size := dataMoveArgs.size.resized

          io.instruction.ready := threeQueue.Readyenqueue3(isDataMove,hostDataFlowHandler.io.into.ready,PortBstrideHandler.io.into.ready,dram1Handler.io.into.ready)

        }
        is(accumulatorToMemory) {
          io.localDataFlow.valid := isDataMove
          io.localDataFlow.payload.size := dataMoveArgs.size.resized
          io.localDataFlow.payload.sel := LocalDataFlowControl.accumulatorToMemory

          PortAstrideHandler.io.into.valid := isDataMove
          PortAstrideHandler.io.into.payload.write := True
          PortAstrideHandler.io.into.payload.stride := dataMoveArgs.memStride
          PortAstrideHandler.io.into.payload.address := dataMoveArgs.memAddress
          PortAstrideHandler.io.into.payload.size := dataMoveArgs.size.resized

          accumulatorHandler.io.into.valid := isDataMove
          accumulatorHandler.io.into.payload.stride := dataMoveArgs.accStride.resized
          accumulatorHandler.io.into.payload.address := dataMoveArgs.accAddress.resized
          accumulatorHandler.io.into.payload.write := False
          accumulatorHandler.io.into.payload.read := True
          accumulatorHandler.io.into.payload.size := dataMoveArgs.size.resized
          accumulatorHandler.io.into.payload.accumulate := False

          io.instruction.ready := threeQueue.Readyenqueue3(isDataMove,io.localDataFlow.ready,PortAstrideHandler.io.into.ready,accumulatorHandler.io.into.ready)
        }
        is(memoryToAccumulator) {
          io.localDataFlow.valid := isDataMove
          io.localDataFlow.payload.size := dataMoveArgs.size.resized
          io.localDataFlow.payload.sel := LocalDataFlowControl.memoryToAccumulator

          PortAstrideHandler.io.into.valid := isDataMove
          PortAstrideHandler.io.into.payload.write := False
          PortAstrideHandler.io.into.payload.stride := dataMoveArgs.memStride
          PortAstrideHandler.io.into.payload.address := dataMoveArgs.memAddress
          PortAstrideHandler.io.into.payload.size := dataMoveArgs.size.resized

          accumulatorHandler.io.into.valid := isDataMove
          accumulatorHandler.io.into.payload.stride := dataMoveArgs.accStride.resized
          accumulatorHandler.io.into.payload.address := dataMoveArgs.accAddress.resized
          accumulatorHandler.io.into.payload.write := True
          accumulatorHandler.io.into.payload.read := False
          accumulatorHandler.io.into.payload.size := dataMoveArgs.size.resized
          accumulatorHandler.io.into.payload.accumulate := False

          io.instruction.ready := threeQueue.Readyenqueue3(isDataMove,io.localDataFlow.ready,PortAstrideHandler.io.into.ready,accumulatorHandler.io.into.ready)
        }

        /* what happens to the accumulate */
        is(memoryToAccumulatorAccumulate) {
          io.localDataFlow.valid := isDataMove
          io.localDataFlow.payload.size := dataMoveArgs.size.resized
          io.localDataFlow.payload.sel := LocalDataFlowControl.memoryToAccumulator

          PortAstrideHandler.io.into.valid := isDataMove
          PortAstrideHandler.io.into.payload.write := False
          PortAstrideHandler.io.into.payload.stride := dataMoveArgs.memStride
          PortAstrideHandler.io.into.payload.address := dataMoveArgs.memAddress
          PortAstrideHandler.io.into.payload.size := dataMoveArgs.size.resized

          accumulatorHandler.io.into.valid := isDataMove
          accumulatorHandler.io.into.payload.stride := dataMoveArgs.accStride.resized
          accumulatorHandler.io.into.payload.address := dataMoveArgs.accAddress.resized
          // Todo
          accumulatorHandler.io.into.payload.write := True
          accumulatorHandler.io.into.payload.read := True
          accumulatorHandler.io.into.payload.size := dataMoveArgs.size.resized
          accumulatorHandler.io.into.payload.accumulate := True

          io.instruction.ready := threeQueue.Readyenqueue3(isDataMove,io.localDataFlow.ready,PortAstrideHandler.io.into.ready,accumulatorHandler.io.into.ready)
        }

      }

    }
  }


  val loadWeight = new Composite(this, "LoadWeight") {
    /* load weight + load zeroes + noop  -> the weight will be loaded into the systolic array */
    /* simple instruction for just load the weight to the Array */
    val isLoad = io.instruction.valid && io.instruction.opcode === Opcode.LoadWeights
    val loadArgs = LoadWeightArgs.fromBits(op0, op1)
    val zeroes = flags(0)
    val loadError = isLoad && !LoadWeightFlags.isValid(flags)

    when(isLoad) {
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
        io.instruction.ready := twoQueue.Readyenqueue2(True, systolicArrayControlHandler.io.into.ready, PortAstrideHandler.io.into.ready)
      }
    }
  }

  val matMul = new Composite(this, "MatMul") {
    /* if zero will run zero and let data move array -> accumulator
    ** if accumulate will output and Memory -> Array -> Acc*/
    val ismatMul = io.instruction.valid && io.instruction.opcode === Opcode.MatMul
    val matMulArgs = MatMulArgs.fromBits(op0, op1, op2)
    val zeroes = flags(0)
    val accumulate = flags(1)
    val matMulError = ismatMul && !MatMulFlags.isValid(flags.asUInt)

    when(ismatMul) {
      /* no care about the alt address */
      accumulatorHandler.io.into.valid := ismatMul
      accumulatorHandler.io.into.payload.size := (matMulArgs.size - 1).resized
      accumulatorHandler.io.into.payload.stride := matMulArgs.accStride
      accumulatorHandler.io.into.payload.reverse := False /* address increase / decrease*/
      accumulatorHandler.io.into.payload.accumulate := False
      accumulatorHandler.io.into.payload.read := False
      accumulatorHandler.io.into.payload.write := True
      accumulatorHandler.io.into.payload.address := matMulArgs.accAddress.resized
      accumulatorHandler.io.into.payload.instruction.op := Opcode.NoOp.asUInt

      io.localDataFlow.payload.size := matMulArgs.size.resized
      io.localDataFlow.valid := ismatMul

      val inputDone = RegInit(False).setWhen(systolicArrayControlHandler.io.into.fire).clearWhen(io.instruction.ready)
      systolicArrayControlHandler.io.into.valid := ismatMul && !inputDone
      systolicArrayControlHandler.io.into.zeroes := zeroes
      systolicArrayControlHandler.io.into.load := False
      systolicArrayControlHandler.io.into.size := (matMulArgs.size - 1).resized

      PortAstrideHandler.io.into.valid := ismatMul && !zeroes
      PortAstrideHandler.io.into.payload.stride := matMulArgs.memStride
      PortAstrideHandler.io.into.payload.address := matMulArgs.memAddress
      PortAstrideHandler.io.into.payload.size := matMulArgs.size.resized
      PortAstrideHandler.io.into.payload.write := False
      when(zeroes) {
        /* running zeroes / no need read the port A */
        io.localDataFlow.payload.sel := LocalDataFlowControl.arrayToAcc
        io.instruction.ready := twoQueue.Readyenqueue2(ismatMul, systolicArrayControlHandler.io.into.ready, accumulatorHandler.io.into.ready)
      }.otherwise {
        /* running input */
        io.localDataFlow.payload.sel := LocalDataFlowControl.memoryToArrayToAcc
        io.instruction.ready := threeQueue.Readyenqueue3(ismatMul, PortAstrideHandler.io.into.ready, systolicArrayControlHandler.io.into.ready, accumulatorHandler.io.into.ready)
      }
    }
  }

  val SIMD = new Composite(this, "SIMD") {
    val isSimd = io.instruction.valid && io.instruction.opcode === Opcode.SIMD
    val simdArgs = SIMDArgs.fromBits(op0,op1,op2)
    val simdRead = flags(2)
    val simdWrite = flags(1)
    val simdAcc = flags(0)
    val simdError = isSimd && !SIMDFlags.isValid(flags.asUInt)

    when(isSimd){
      accumulatorHandler.io.into.valid := isSimd
      accumulatorHandler.io.into.payload.size.clearAll()
      accumulatorHandler.io.into.payload.stride.clearAll()
      accumulatorHandler.io.into.payload.reverse := False
      accumulatorHandler.io.into.payload.accumulate := simdAcc
      accumulatorHandler.io.into.payload.read := simdRead
      accumulatorHandler.io.into.payload.write := simdWrite
      accumulatorHandler.io.into.payload.address := simdArgs.accReadAddress.resized
      accumulatorHandler.io.into.payload.altAddress:= simdArgs.accWriteAddress.resized
      accumulatorHandler.io.into.payload.instruction.dest := simdArgs.instruction.dest
      accumulatorHandler.io.into.payload.instruction.op := simdArgs.instruction.op
      accumulatorHandler.io.into.payload.instruction.sourceLeft := simdArgs.instruction.sourceLeft
      accumulatorHandler.io.into.payload.instruction.sourceRight := simdArgs.instruction.sourceLeft

      io.instruction.ready := accumulatorHandler.io.into.ready
    }
  }

  val configure = new Composite(this, "Configure") {

    import Configure._

    /* the configure instruction is used to configure some regs in the NPU */
    val isConfigure = io.instruction.valid && io.instruction.opcode === Opcode.Configure
    val configureArgs = ConfigureArgs(op1, op0)
    val configureError = isConfigure && !isValid(op0.asUInt)

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
        default {
          io.instruction.ready := True
        }
      }
    }
  }



  val HasError = RegInit(False).setWhen(
    io.instruction.valid && Opcode.Operror(io.instruction.opcode) || loadWeight.loadError || configure.configureError
      || matMul.matMulError || dataMove.dataMoveError || SIMD.simdError
  )

  io.error := HasError
  io.pc := pc
}

object Decode extends App {
  val arch = Architecture.tiny()
  val layOut = InstructionLayOut(arch)
  SpinalSystemVerilog(new Decode(arch)(layOut))
}