package Betsy


/**
 * * Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 * * Update Time : 2024/6/6      SpinalHDL Version: 1.94      **
 * * You should have received a copy of the MIT License along with this library **
 * * **
 */


import spinal.core._
import spinal.lib._
import BetsyLibs._
import Betsy.Instruction._
import Betsy.Until._
import spinal.core.sim._
import Architecture._
/** Decode test order : NoOp -> DataMove -> LoadWeight -> MatMul -> Configure -> SIMD
 * this version instruction only enqueue when fire (so the instruction is blocked when before instruction not done !!!)
 * */

case class dramAddressOffset(addressWith:Int) extends Bundle {
  val offset = UInt(addressWith bits)
}

class Decode(arch: Architecture)(implicit layOut: InstructionLayOut) extends BetsyModule {

  val io = new Bundle {
    val instructionFormat = slave Stream InstructionFormat(layOut.instructionSizeBytes * 8)
    val dram0 = master Stream MemControl(arch.dram0Depth)
    val dram0offset = out (dramAddressOffset(getWeightBusConfig(arch).addressWidth))
    val dram1 = master Stream MemControl(arch.dram0Depth)
    val dram1offset = out (dramAddressOffset(getActivationBusConfig(arch).addressWidth))
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

  val instruction = Stream(InstructionFormat(layOut.instructionSizeBytes * 8)) // work loads with one cycle pipeline later
  instruction.simPublic()
  io.instructionFormat >> instruction

  val pc = Reg(UInt(arch.pcWidth bits)).init(0)
  when(instruction.fire) {
    pc := pc + layOut.instructionSizeBytes /* 8 or 4 bytes better */
  }

  val interval = Reg(UInt(arch.pcWidth / 2 bits)).init(0)
  val runCycles = Reg(UInt(64 bits)).init(0)
  when(instruction.valid) {
    runCycles := runCycles + 1
  }

  val opcode = instruction.payload.opcode
  val flags = instruction.payload.flags
  val arguments = instruction.payload.arguments
  val op0 = arguments(layOut.operand0SizeBits - 1 downto 0)
  val op1 = arguments(layOut.operand0SizeBits + layOut.operand1SizeBits - 1 downto layOut.operand0SizeBits)
  val op2 = arguments(layOut.operandsSizeBits - 1 downto layOut.operand0SizeBits + layOut.operand1SizeBits)

  // the systolic array control with size
  val systolicArrayControlHandler = new SizeHandler(new SystolicArrayControlWithSize(arch.localDepth),
    SystolicArrayControl(),
    arch.localDepth)
  systolicArrayControlHandler.io.output >> io.systolicArrayControl
  setDefault(systolicArrayControlHandler.io.into)

  // control the portA
  val PortAstrideHandler = new StrideHandler(new MemControlWithStride(arch.localDepth, arch.stride0Depth),
    MemControl(arch.localDepth),
    arch.localDepth)
  PortAstrideHandler.io.output >> io.memPortA
  setDefault(PortAstrideHandler.io.into)

  //control the portB
  val PortBstrideHandler = new StrideHandler(new MemControlWithStride(arch.localDepth, arch.stride1Depth),
    MemControl(arch.localDepth),
    arch.localDepth)
  PortBstrideHandler.io.output >> io.memPortB
  setDefault(PortBstrideHandler.io.into)

  // accumulator stride/size handler
  val accumulatorHandler = new StrideHandler(new AccumulatorMemControlWithSizeWithStride(layOut),
    AccumulatorMemControl(layOut),
    arch.accumulatorDepth)
  accumulatorHandler.io.output.payload.toAccumulatorWithALUArrayControl(arch) <> io.accumulatorWithALUArrayControl.payload
  io.accumulatorWithALUArrayControl.arbitrationFrom(accumulatorHandler.io.output)
  setDefault(accumulatorHandler.io.into)

  val dram0Handler = new StrideHandler(new MemControlWithStride(arch.dram0Depth, arch.stride0Depth),
    MemControl(arch.dram0Depth),
    arch.dram0Depth)
  dram0Handler.io.output >> io.dram0
  setDefault(dram0Handler.io.into)

  val dram1Handler = new StrideHandler(new MemControlWithStride(arch.dram1Depth, arch.stride1Depth),
    MemControl(arch.dram1Depth),
    arch.dram1Depth)
  dram1Handler.io.output >> io.dram1
  setDefault(dram1Handler.io.into)

  val hostDataFlowHandler = new SizeHandler(new HostDataFlowControlWithSize(arch.localDepth),
    new HostDataFlowControl(), arch.localDepth)
  hostDataFlowHandler.io.output >> io.hostDataFlow
  setDefault(hostDataFlowHandler.io.into)

  val twoQueue = MultiEnqControl(2)
  val threeQueue = MultiEnqControl(3)
  val fourQueue = MultiEnqControl(4)
  val portAdown = RegInit(False).setWhen(PortAstrideHandler.io.into.fire).clearWhen(instruction.ready)
  val portBdown = RegInit(False).setWhen(PortAstrideHandler.io.into.fire).clearWhen(instruction.ready)
  val dram0down = RegInit(False).setWhen(dram0Handler.io.into.fire).clearWhen(instruction.ready)
  val dram1down = RegInit(False).setWhen(dram1Handler.io.into.fire).clearWhen(instruction.ready)
  val arraycontroldown = RegInit(False).setWhen(systolicArrayControlHandler.io.into.fire).clearWhen(instruction.ready)
  val localcontroldown = RegInit(False).setWhen(io.localDataFlow.fire).clearWhen(instruction.ready)
  val hostcontroldown = RegInit(False).setWhen(hostDataFlowHandler.io.into.fire).clearWhen(instruction.ready)
  val accumulatordown = RegInit(False).setWhen(accumulatorHandler.io.into.fire).clearWhen(instruction.ready)
  val inputDone = RegInit(False).setWhen(systolicArrayControlHandler.io.into.fire).clearWhen(instruction.ready)

  val NoOp = new Composite(this, "NoOp") {
    io.localDataFlow.valid.clear()
    io.localDataFlow.payload.clearAll()
    instruction.ready.clear()
    io.dram0offset.clearAll()
    io.dram1offset.clearAll()
    // build the nop instruction
    val isNoOp = instruction.valid && instruction.payload.opcode === Opcode.NoOp
    when(isNoOp) {
      /* with the op instructions running */
      io.nop := True
      instruction.ready := True
    }.otherwise {
      io.nop := False
    }
  }

  //build the data move instruction path
  val dataMove = new Composite(this, "DataMove") {
    val isDataMove = instruction.valid && instruction.payload.opcode === Opcode.DataMove
    val dataMoveArgs = DataMoveArgs.fromBits(op0, op1, op2)
    val dataMoveFlags = DataMoveFlags()
    dataMoveFlags.kind := flags.asUInt
    val dataMoveError = isDataMove && !DataMoveKind.isValid(flags.asUInt)
    val dataMoveValidError = RegInit(False).setWhen(!DataMoveKind.isValid(dataMoveFlags.kind) && isDataMove)

    import DataMoveKind._

    val DramOperation = isDataMove && (flags.asUInt === dram0ToMemory || flags.asUInt === dram1ToMemory  ||
      flags.asUInt === memoryToDram0 || flags.asUInt === memoryToDram1)

    val calculate = new Area{
      when(DramOperation){
        val baseAddress = dataMoveArgs.accAddress
        val shifterNum = U(log2Up(arch.arraySize))
        val offset = baseAddress << shifterNum
        io.dram0offset.offset := offset.resized
        io.dram1offset.offset := offset.resized
      }
    }

    when(isDataMove) {
      /* using the flags show */
      switch(flags.asUInt) {
        is(dram0ToMemory) { //from the DRAM0 to the memory
          hostDataFlowHandler.io.into.valid := !hostcontroldown
          val hostDataFlowControlWithSize = HostDataFlowControlWithSize(arch.localDepth,
            size = dataMoveArgs.size.resized,
            kind = HostDataFlowControl.In0
          )
          hostDataFlowControlWithSize <> hostDataFlowHandler.io.into.payload

          PortBstrideHandler.io.into.valid := !portBdown
          val portBMemControlWithStride = MemControlWithStride(arch.localDepth,
            arch.stride1Depth,
            write = True,
            address = dataMoveArgs.memAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.memStride)
          PortBstrideHandler.io.into.payload <> portBMemControlWithStride

          dram0Handler.io.into.valid := !dram0down
          val dram0MemControlWithStride = MemControlWithStride(arch.dram0Depth,
            arch.stride0Depth,
            write = False,
            address = dataMoveArgs.accAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.accStride)
          dram0Handler.io.into.payload <> dram0MemControlWithStride

          instruction.ready := threeQueue.Readyenqueue3(isDataMove,
            hostDataFlowHandler.io.into.ready,
            PortBstrideHandler.io.into.ready,
            dram0Handler.io.into.ready)
        }

        is(memoryToDram0) { // from memory to dram0
          hostDataFlowHandler.io.into.valid.set()
          val hostDataFlowControlWithSize = HostDataFlowControlWithSize(arch.localDepth,
            size = dataMoveArgs.size.resized,
            kind = HostDataFlowControl.Out0
          )
          hostDataFlowControlWithSize <> hostDataFlowHandler.io.into.payload

          PortBstrideHandler.io.into.valid := isDataMove
          val portBMemControlWithStride = MemControlWithStride(arch.localDepth,
            arch.stride1Depth,
            write = False,
            address = dataMoveArgs.memAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.memStride)
          PortBstrideHandler.io.into.payload <> portBMemControlWithStride

          dram0Handler.io.into.valid := isDataMove
          val dram0MemControlWithStride = MemControlWithStride(arch.dram0Depth,
            arch.stride0Depth,
            write = True,
            address = dataMoveArgs.accAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.accStride)
          dram0Handler.io.into.payload <> dram0MemControlWithStride

          instruction.ready := threeQueue.Readyenqueue3(isDataMove,
            hostDataFlowHandler.io.into.ready,
            PortBstrideHandler.io.into.ready,
            dram0Handler.io.into.ready)
        }

        is(dram1ToMemory) { // from dram1 to the memory
          hostDataFlowHandler.io.into.valid.set()
          val hostDataFlowControlWithSize = HostDataFlowControlWithSize(arch.localDepth,
            size = dataMoveArgs.size.resized,
            kind = HostDataFlowControl.In1
          )
          hostDataFlowControlWithSize <> hostDataFlowHandler.io.into.payload

          PortBstrideHandler.io.into.valid := isDataMove
          val portBMemControlWithStride = MemControlWithStride(arch.localDepth,
            arch.stride1Depth,
            write = True,
            address = dataMoveArgs.memAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.memStride)
          PortBstrideHandler.io.into.payload <> portBMemControlWithStride

          dram1Handler.io.into.valid := isDataMove
          val dram1MemControlWithStride = MemControlWithStride(arch.dram1Depth,
            arch.stride1Depth,
            write = False,
            address = dataMoveArgs.accAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.accStride)
          dram1Handler.io.into.payload <> dram1MemControlWithStride

          instruction.ready := threeQueue.Readyenqueue3(isDataMove,
            hostDataFlowHandler.io.into.ready,
            PortBstrideHandler.io.into.ready,
            dram1Handler.io.into.ready)
        }

        is(memoryToDram1) { // from memory to dram1
          hostDataFlowHandler.io.into.valid.set()
          val hostDataFlowControlWithSize = HostDataFlowControlWithSize(arch.localDepth,
            size = dataMoveArgs.size.resized,
            kind = HostDataFlowControl.Out1
          )
          hostDataFlowControlWithSize <> hostDataFlowHandler.io.into.payload

          PortBstrideHandler.io.into.valid := isDataMove
          val portBMemControlWithStride = MemControlWithStride(arch.localDepth,
            arch.stride1Depth,
            write = False,
            address = dataMoveArgs.memAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.memStride)
          PortBstrideHandler.io.into.payload <> portBMemControlWithStride

          dram1Handler.io.into.valid := isDataMove
          val dram1MemControlWithStride = MemControlWithStride(arch.dram1Depth,
            arch.stride1Depth,
            write = True,
            address = dataMoveArgs.accAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.accStride)
          dram1Handler.io.into.payload <> dram1MemControlWithStride

          instruction.ready := threeQueue.Readyenqueue3(isDataMove,
            hostDataFlowHandler.io.into.ready,
            PortBstrideHandler.io.into.ready,
            dram1Handler.io.into.ready)
        }

        is(accumulatorToMemory) { // form the accumulator -> memory
          io.localDataFlow.valid := isDataMove
          val localDataFlowControlWithSize = LocalDataFlowControlWithSize(arch.localDepth,
            sel = LocalDataFlowControl.accumulatorToMemory,
            size = dataMoveArgs.size.resized)
          io.localDataFlow.payload <> localDataFlowControlWithSize

          PortAstrideHandler.io.into.valid := isDataMove
          val portAMemControlWithStride = MemControlWithStride(arch.localDepth,
            arch.stride0Depth,
            write = True,
            address = dataMoveArgs.memAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.memStride)
          PortAstrideHandler.io.into.payload <> portAMemControlWithStride

          accumulatorHandler.io.into.valid := isDataMove
          val accumulatorMemControlWithSizeWithStride = AccumulatorMemControlWithSizeWithStride(layOut,
            size = dataMoveArgs.size.resized,
            stride = dataMoveArgs.accStride.resized,
            reverse = False,
            accumulate = False,
            read = True,
            write = False,
            address = dataMoveArgs.accAddress.resized,
            instruction = SIMDInstruction.noOp(),
            altAddress = U(0).resized
          )
          accumulatorMemControlWithSizeWithStride <> accumulatorHandler.io.into.payload

          instruction.ready := threeQueue.Readyenqueue3(isDataMove,
            io.localDataFlow.ready,
            PortAstrideHandler.io.into.ready,
            accumulatorHandler.io.into.ready)
        }

        is(memoryToAccumulator) { // memory -> accumulator
          io.localDataFlow.valid := isDataMove
          val localDataFlowControlWithSize = LocalDataFlowControlWithSize(arch.localDepth,
            sel = LocalDataFlowControl.memoryToAccumulator,
            size = dataMoveArgs.size.resized)
          io.localDataFlow.payload <> localDataFlowControlWithSize

          PortAstrideHandler.io.into.valid := isDataMove
          val portAMemControlWithStride = MemControlWithStride(arch.localDepth,
            arch.stride0Depth,
            write = False,
            address = dataMoveArgs.memAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.memStride)
          PortAstrideHandler.io.into.payload <> portAMemControlWithStride

          accumulatorHandler.io.into.valid := isDataMove
          val accumulatorMemControlWithSizeWithStride = AccumulatorMemControlWithSizeWithStride(layOut,
            size = dataMoveArgs.size.resized,
            stride = dataMoveArgs.accStride.resized,
            reverse = False,
            accumulate = False,
            read = False,
            write = True,
            address = dataMoveArgs.accAddress.resized,
            instruction = SIMDInstruction.noOp(),
            altAddress = U(0).resized
          )
          accumulatorHandler.io.into.payload <> accumulatorMemControlWithSizeWithStride

          instruction.ready := threeQueue.Readyenqueue3(isDataMove,
            io.localDataFlow.ready,
            PortAstrideHandler.io.into.ready,
            accumulatorHandler.io.into.ready)
        }

        /* what happens to the accumulate */
        is(memoryToAccumulatorAccumulate) {
          io.localDataFlow.valid := isDataMove
          val localDataFlowControlWithSize = LocalDataFlowControlWithSize(arch.localDepth,
            sel = LocalDataFlowControl.memoryToAccumulator,
            size = dataMoveArgs.size.resized)
          io.localDataFlow.payload <> localDataFlowControlWithSize

          PortAstrideHandler.io.into.valid := isDataMove
          val portAMemControlWithStride = MemControlWithStride(arch.localDepth,
            arch.stride0Depth,
            write = False,
            address = dataMoveArgs.memAddress,
            size = dataMoveArgs.size.resized,
            reverse = False,
            stride = dataMoveArgs.memStride)
          PortAstrideHandler.io.into.payload <> portAMemControlWithStride

          accumulatorHandler.io.into.valid := isDataMove
          val accumulatorMemControlWithSizeWithStride = AccumulatorMemControlWithSizeWithStride(layOut,
            size = dataMoveArgs.size.resized,
            stride = dataMoveArgs.accStride.resized,
            reverse = False,
            accumulate = True,
            read = False,
            write = True,
            address = dataMoveArgs.accAddress.resized,
            instruction = SIMDInstruction.noOp(),
            altAddress = U(0).resized
          )
          accumulatorHandler.io.into.payload <> accumulatorMemControlWithSizeWithStride

          instruction.ready := threeQueue.Readyenqueue3(isDataMove,
            io.localDataFlow.ready,
            PortAstrideHandler.io.into.ready,
            accumulatorHandler.io.into.ready)
        }
      }
    }
  }


  val loadWeight = new Composite(this, "LoadWeight") {
    /* load weight + load zeroes + noop  -> the weight will be loaded into the systolic array */
    /* simple instruction for just load the weight to the Array */
    val isLoad = instruction.valid && instruction.payload.opcode === Opcode.LoadWeights
    val loadArgs = LoadWeightArgs.fromBits(op0, op1)
    val zeroes = flags(0)
    val loadError = isLoad && !LoadWeightFlags.isValid(flags)

    when(isLoad) {
      /* load control signals */
      PortAstrideHandler.io.into.valid := (!zeroes) && (!portAdown)
      val portAMemControlWithStride = MemControlWithStride(arch.localDepth,
        arch.stride0Depth,
        write = False,
        address = loadArgs.address,
        size = loadArgs.size.resized,
        reverse = False,
        stride = loadArgs.stride)
      PortAstrideHandler.io.into.payload <> portAMemControlWithStride

      systolicArrayControlHandler.io.into.valid := !arraycontroldown
      val systolicArrayControlWithSize = SystolicArrayControlWithSize(arch.localDepth,
        load = isLoad,
        zeroes = zeroes,
        size = loadArgs.size.resized)
      systolicArrayControlHandler.io.into.payload <> systolicArrayControlWithSize

      io.localDataFlow.valid := (!zeroes) && (!localcontroldown)
      val localDataFlowControlWithSize = LocalDataFlowControlWithSize(arch.localDepth,
        sel = LocalDataFlowControl.memoryToArrayWeight,
        size = loadArgs.size.resized)
      io.localDataFlow.payload <> localDataFlowControlWithSize

      when(zeroes) {
        instruction.ready := systolicArrayControlHandler.io.into.ready
      }.otherwise {
        instruction.ready := threeQueue.Readyenqueue3(isLoad,
          io.localDataFlow.ready,
          systolicArrayControlHandler.io.into.ready,
          PortAstrideHandler.io.into.ready)
      }
    }
  }

  val matMul = new Composite(this, "MatMul") {
    /* if zero will run zero and let data move array -> accumulator
    ** if accumulate will output and Memory -> Array -> Acc*/
    val ismatMul = instruction.valid && instruction.payload.opcode === Opcode.MatMul
    val matMulArgs = MatMulArgs.fromBits(op0, op1, op2)
    val zeroes = flags(0)
    val accumulate = flags(1)
    val matMulError = ismatMul && !MatMulFlags.isValid(flags.asUInt)

    when(ismatMul) {
      /* no care about the alt address */
      accumulatorHandler.io.into.valid := !accumulatordown
      val accumulatorMemControlWithSizeWithStride = AccumulatorMemControlWithSizeWithStride(layOut,
        size = matMulArgs.size.resized,
        stride = matMulArgs.accStride,
        reverse = False,
        accumulate = accumulate,
        read = False,
        write = True,
        address = matMulArgs.accAddress.resized,
        instruction = SIMDInstruction.noOp(),
        altAddress = U(0).resized
      )
      accumulatorMemControlWithSizeWithStride <> accumulatorHandler.io.into.payload

      systolicArrayControlHandler.io.into.valid := !inputDone
      val systolicArrayControlWithSize = SystolicArrayControlWithSize(arch.localDepth,
        load = False,
        zeroes = zeroes,
        size = matMulArgs.size.resized)
      systolicArrayControlHandler.io.into.payload <> systolicArrayControlWithSize

      PortAstrideHandler.io.into.valid := (!zeroes) && (!portAdown)
      val portAMemControl = MemControlWithStride(arch.localDepth,
        arch.stride0Depth,
        write = False,
        address = matMulArgs.memAddress,
        size = matMulArgs.size.resized,
        reverse = False,
        stride = matMulArgs.memStride)
      PortAstrideHandler.io.into.payload <> portAMemControl

      io.localDataFlow.payload.size := matMulArgs.size.resized
      io.localDataFlow.valid := !localcontroldown
      when(zeroes) {
        /* running zeroes / no need read the port A */
        io.localDataFlow.payload.sel := LocalDataFlowControl.arrayToAcc
        instruction.ready := threeQueue.Readyenqueue3(ismatMul,
          io.localDataFlow.ready,
          systolicArrayControlHandler.io.into.ready,
          accumulatorHandler.io.into.ready)
      }.otherwise {
        /* running input */
        io.localDataFlow.payload.sel := LocalDataFlowControl.memoryToArrayToAcc
        instruction.ready := fourQueue.Readyenqueue4(ismatMul,
          io.localDataFlow.ready,
          PortAstrideHandler.io.into.ready,
          systolicArrayControlHandler.io.into.ready,
          accumulatorHandler.io.into.ready)
      }
    }
  }

  val SIMD = new Composite(this, "SIMD") {
    val isSimd = instruction.valid && instruction.payload.opcode === Opcode.SIMD
    val simdArgs = SIMDArgs.fromBits(op0, op1, op2)
    val simdRead = flags(2)
    val simdWrite = flags(1)
    val simdAcc = flags(0)
    val simdError = isSimd && !SIMDFlags.isValid(flags.asUInt)

    when(isSimd) {
      accumulatorHandler.io.into.valid := isSimd
      val simdInstruction = SIMDInstruction(simdArgs.instruction.op,
        simdArgs.instruction.sourceLeft,
        simdArgs.instruction.sourceRight,
        simdArgs.instruction.dest)
      val accumulatorMemControlWithSizeWithStride = AccumulatorMemControlWithSizeWithStride(layOut,
        size = U(0).resized,
        stride = U(0).resized,
        reverse = False,
        accumulate = simdAcc,
        read = simdRead,
        write = simdWrite,
        address = simdArgs.accReadAddress.resized,
        instruction = simdInstruction,
        altAddress = simdArgs.accWriteAddress.resized
      )
      accumulatorMemControlWithSizeWithStride <> accumulatorHandler.io.into.payload
      instruction.ready := accumulatorHandler.io.into.ready
    }
  }

  val configure = new Composite(this, "Configure") {

    import Configure._

    /* the configure instruction is used to configure some regs in the NPU */
    val isConfigure = instruction.valid && instruction.payload.opcode === Opcode.Configure
    val configureArgs = ConfigureArgs(op1, op0)
    val configureError = isConfigure && !isValid(op0.asUInt)

    when(isConfigure) {
      switch(configureArgs.register.asUInt) {
        is(programCounter) {
          pc := configureArgs.value.asUInt.resized
          instruction.ready := True
        }
        is(runningCycles) {
          runCycles := configureArgs.value.asUInt.resized
          instruction.ready := True
        }
        is(sampleInterval) {
          interval := configureArgs.value.asUInt.resized
          instruction.ready := True
        }
        default {
          instruction.ready := True
        }
      }
    }
  }

  val control = new Composite(this, "Control") {
    /* control the pc / deal with the error / support the sampler unit / keep the run cycles*/
    val strideHandlerError = PortAstrideHandler.io.error || PortBstrideHandler.io.error || accumulatorHandler.io.error ||
      dram0Handler.io.error || dram1Handler.io.error
    val HasError = RegInit(False).setWhen(
      instruction.valid && Opcode.Operror(instruction.payload.opcode) || loadWeight.loadError || configure.configureError
        || matMul.matMulError || dataMove.dataMoveError || SIMD.simdError || strideHandlerError
    )

    arch.withSampler generate{
      new Composite(this, "Sampler"){
        val run = OHToUInt(Seq(NoOp.isNoOp,loadWeight.isLoad,matMul.ismatMul,dataMove.isDataMove,SIMD.isSimd,configure.isConfigure).reverse)
        val sampler = new Sampler(arch = arch)
        val flags = SamplerFlags()
        val samplerOut = Bool()
        sampler.io.pc := pc
        sampler.io.interval := interval
        sampler.io.flags.valid := instruction.valid
        sampler.io.flags.ready := instruction.ready
        sampler.io.flags.instruction.assignFromBits(run.asBits)
        sampler.io.sample.ready := True
        samplerOut := sampler.io.sample.valid
        flags <> sampler.io.sample.payload.flags
      }
    }
    io.error := HasError
    io.pc := pc
  }
}

//object Decode extends App {
//  val arch = Architecture.tiny()
//  val layOut = InstructionLayOut(arch)
//  SpinalSystemVerilog(new Decode(arch)(layOut))
//}