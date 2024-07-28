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

 /* here will be some operations like
  * (1) just read from the accumulator
  * (2) just write into the accumulator
  * (3) write and accumulate into the accumulator */

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

  val isNoOp = io.control.payload.SIMDInstruction.op === ALUOp.NoOp
  val dataPathReady = False
  val readEnqueued = RegInit(False)
  readEnqueued := False

  val accWriteEnqueuer = MultiEnqControl(2)
  val accReadEnqueuer =  MultiEnqControl(2)
  val simdRWWriteEnqueuer = MultiEnqControl(3)
  val simdRWReadEnqueuer = MultiEnqControl(3)
  val simdWriteEnqueuer = MultiEnqControl(4)
  val simdReadEnqueuer = MultiEnqControl(4)
  val simdEnqueuer = MultiEnqControl(2)

  when(isNoOp) {
    // the noop is about the acc read and acc write operation
    when(control.payload.read) {
      when(control.payload.write) {
        when(readEnqueued) { // when read and write - must wait the read operation finish
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
        dataPathReady := accReadEnqueuer.enqueue2( // just read operation
          control.valid,
          accumulator.io.control,
          readControl(),
          accOutDemux,
          U(0,1 bits)
        )
      }
    }.otherwise {
      when(control.payload.write) { // just write operation
        dataPathReady := accWriteEnqueuer.enqueue2(
          control.valid,
          accumulator.io.control,
          writeControl(),
          accInMux,
          U(0,1 bits)
        )
      }.otherwise {
        dataPathReady := True // noop and no read and write
      }
    }
    control.ready := dataPathReady
  }.otherwise {
    /* the alu operations with simd operations
    * no read and no write -> just with alu output (sink)
    * no read and write -> the alu out and write into the accumulator
    * read and no write -> read from the accumulator and into the alu array (finally alu will out)
    * read and write -> first read the accumulator value and then write the accumulator (may be with accumulate operation )
    * and when write behaviour happens -> also support accumulate in the accumulator */
    when(control.payload.read) {
      when(control.payload.write) {
        // first read, then write
        when(readEnqueued) {
          dataPathReady := simdRWWriteEnqueuer.enqueue3( // from the alu out and write into the accumulator
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
            readEnqueued := simdRWReadEnqueuer.enqueue3( // read the accumulator value
              control.valid,
              accumulator.io.control,
              readControl(),
              accOutDemux,
              U(1, 1 bits),
              aluArray.io.instruction,
              control.payload.SIMDInstruction
            )
            aluOutDemux.payload := U(0, 1 bits)
            aluOutDemux.valid := True
          }.otherwise {
            readEnqueued := True
          }
        }
      }.otherwise {
        dataPathReady := simdReadEnqueuer.enqueue4( // read from the accumulator and into the alu array (finally alu will out)
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
        dataPathReady := simdEnqueuer.enqueue2( // just alu output(sink)
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
  SpinalVerilog(new AccumulatorWithALUArray(SInt(8 bits),Architecture.tiny()))
}