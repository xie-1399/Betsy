package Betsy

import Betsy.Until.BetsyModule
import BetsyLibs._
import spinal.core._
import spinal.lib._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/6/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** combine the alu array and teh accumulator with control    **
 */

//Todo test it
class AccumulatorWithALUArray[T <: Data with Num[T]](gen:HardType[T],arch: Architecture) extends BetsyModule {
  def simdHeight = arch.arraySize
  val layOut =  InstructionLayOut(arch)

  val io = new Bundle {
    val inputs = slave(Stream(Vec(gen, simdHeight)))
    val outputs = master(Stream(Vec(gen, simdHeight)))
    val control = slave(Stream(AccumulatorWithALUArrayControl(layOut)))
  }

  def writeControl(): AccumulatorControl = {
    val w = cloneOf(accumulator.io.control.payload)
    w.address := io.control.writeAddress
    w.write := True
    w.accumulate := io.control.accumulate
    w
  }

  def readControl(): AccumulatorControl = {
    val w = cloneOf(accumulator.io.control.payload)
    w.address := io.control.readAddress
    w.write := False
    w.accumulate := False
    w
  }

  def tieOff[T <: Data](stream: Stream[T]): Unit = {
    stream.valid := False
    stream.payload.clearAll()
  }

  val accumulator = new Accumulator(gen, simdHeight, arch.accumulatorDepth)
  val aluArray = new ALUArray(gen, arch)
  val aluOutputSink = new Sink(cloneOf(aluArray.io.outputs.payload))
  val aluOutPut = aluArray.io.outputs
  val aluOutputForAccInput = Stream(cloneOf(aluArray.io.outputs.payload))
  val inputs = io.inputs
  val control = io.control

  /* with the sel signals control */
  val aluOutDemux = BetsyStreamDemux(aluOutPut, aluOutputSink.io.into, aluOutputForAccInput)
  val accInMux = BetsyStreamMux(inputs, aluOutputForAccInput, accumulator.io.dataIn)
  val accOutDemux = BetsyStreamDemux(accumulator.io.dataOut, io.outputs, aluArray.io.inputs)
  tieOff(aluOutDemux)
  tieOff(accInMux)
  tieOff(accOutDemux)
  tieOff(accumulator.io.control)
  tieOff(aluArray.io.instruction)

  val readEnqueued = RegInit(False)
  readEnqueued := False

  val accWriteEnqueuer = new MultiEnqControl(2)
  val accReadEnqueuer = new MultiEnqControl(2)
  val simdRWWriteEnqueuer = new MultiEnqControl(3)
  val simdRWReadEnqueuer = new MultiEnqControl(3)
  val simdWriteEnqueuer = new MultiEnqControl(4)
  val simdReadEnqueuer = new MultiEnqControl(4)
  val simdEnqueuer = new MultiEnqControl(2)
  accWriteEnqueuer.block()
  accReadEnqueuer.block()
  simdRWWriteEnqueuer.block()
  simdRWReadEnqueuer.block()
  simdWriteEnqueuer.block()
  simdReadEnqueuer.block()
  simdEnqueuer.block()

  val isNoOp = io.control.payload.SIMDInstruction.op.asBits === Opcode.NoOp

  when(isNoOp) {
    val dataPathReady = False
    when(control.payload.read) {
      when(control.payload.write) {
        when(readEnqueued) {
          dataPathReady := accWriteEnqueuer.enqueue2(
            control.valid,
            accumulator.io.control,
            writeControl(),
            accInMux,
            U(0,1 bits)
          )
          when(dataPathReady) {
            readEnqueued := False
          }.otherwise {
            readEnqueued := readEnqueued
          }
        }.otherwise {
          dataPathReady := False
          readEnqueued := accReadEnqueuer.enqueue2(
            control.valid,
            accumulator.io.control,
            readControl(),
            accOutDemux,
            U(0,1 bits)
          )
        }
      }.otherwise {
        dataPathReady := accReadEnqueuer.enqueue2(
          control.valid,
          accumulator.io.control,
          readControl(),
          accOutDemux,
          U(0,1 bits)
        )
      }
    }.otherwise {
      when(control.payload.write) {
        dataPathReady := accWriteEnqueuer.enqueue2(
          control.valid,
          accumulator.io.control,
          writeControl(),
          accInMux,
          U(0,1 bits)
        )
      }.otherwise {
        dataPathReady := True
      }
    }
    control.ready := dataPathReady
  }.otherwise {
    val dataPathReady = False
    when(control.payload.read) {
      when(control.payload.write) {
        // first read, then write
        when(readEnqueued) {
          dataPathReady := simdRWWriteEnqueuer.enqueue3(
            control.valid,
            accumulator.io.control,
            writeControl(),
            aluOutDemux,
            U(1, 1 bits),
            accInMux,
            U(1, 1 bits)
          )
          when(dataPathReady) {
            readEnqueued := False
          }.otherwise {
            readEnqueued := readEnqueued
          }
        }.otherwise {
          dataPathReady := False
          when(
            control.payload.SIMDInstruction.sourceLeft === 0 || control.payload.SIMDInstruction.sourceRight === 0
          ) {
            readEnqueued := simdRWReadEnqueuer.enqueue3(
              control.valid,
              accumulator.io.control,
              readControl(),
              accOutDemux,
              U(1,1 bits),
              aluArray.io.instruction,
              control.payload.SIMDInstruction
            )
          }.otherwise {
            readEnqueued := True
          }
        }
      }.otherwise {
        dataPathReady := simdReadEnqueuer.enqueue4(
          control.valid,
          accumulator.io.control,
          readControl(),
          accOutDemux,
          U(1,1 bits),
          aluOutDemux,
          U(0,1 bits),
          aluArray.io.instruction,
          control.payload.SIMDInstruction
        )
      }
    }.otherwise {
      when(control.payload.write) {
        dataPathReady := simdWriteEnqueuer.enqueue4(
          control.valid,
          accumulator.io.control,
          writeControl(),
          aluOutDemux,
          U(1,1 bits),
          accInMux,
          U(1,1 bits),
          aluArray.io.instruction,
          control.payload.SIMDInstruction
        )
      }.otherwise {
        dataPathReady := simdEnqueuer.enqueue2(
          control.valid,
          aluOutDemux,
          U(0,1 bits),
          aluArray.io.instruction,
          control.payload.SIMDInstruction
        )
      }
    }
    control.ready := dataPathReady
  }
}


object AccumulatorWithALUArray extends App{
  SpinalVerilog(new AccumulatorWithALUArray(SInt(4 bits),Architecture.tiny()))
}