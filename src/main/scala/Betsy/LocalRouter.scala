package Betsy

/**
 * * Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 * * Update Time : 2024/4/13      SpinalHDL Version: 1.94      **
 * * You should have received a copy of the MIT License along with this library **
 * * the local router control the data move between Accumulator/ Array / Scratch Pad **
 * * Test Status : PASS :)         Version:0.1 **
 */

import Betsy.Until._
import BetsyLibs._
import spinal.core._
import spinal.lib._

/* the dataflow only controls the data move process */
case class MemoryDataFlow[T <: Data](gen: HardType[T]) extends Bundle with IMasterSlave {
  val memIn = Stream(gen())
  val memOut = Stream(gen())

  override def asMaster(): Unit = {
    master(memOut)
    slave(memIn)
  }
}

case class AccumulatorDataFlow[T <: Data](gen: HardType[T]) extends Bundle with IMasterSlave {
  val accIn = Stream(gen())
  val accOut = Stream(gen())

  override def asMaster(): Unit = {
    master(accOut)
    slave(accIn)
  }
}

case class ArrayDataFlow[T <: Data](gen: HardType[T]) extends Bundle with IMasterSlave {
  val weight = Stream(gen())
  val input = Stream(gen())
  val output = Stream(gen())

  override def asMaster(): Unit = {
    master(output)
    slave(input, weight)
  }
}


class LocalRouter[T <: Data](gen: HardType[T], arch: Architecture) extends BetsyModule {

  val io = new Bundle {
    val control = slave(Stream(new LocalDataFlowControlWithSize(arch.localDepth)))
    val memoryDataFlow = slave(MemoryDataFlow(gen))
    val arrayDataFlow = slave(ArrayDataFlow(gen))
    val accumulatorDataFlow = slave(AccumulatorDataFlow(gen))
  }

  def IsMemRead(sel: UInt): Bool = {
    val select = sel === LocalDataFlowControl.memoryToAccumulator || sel === LocalDataFlowControl.memoryToArrayToAcc ||
      sel === LocalDataFlowControl.memoryToArrayWeight
    select
  }

  def IsWriteAcc(sel: UInt): Bool = {
    val select = sel === LocalDataFlowControl.arrayToAcc || sel === LocalDataFlowControl.memoryToAccumulator ||
      sel === LocalDataFlowControl.memoryToArrayToAcc
    select
  }

  def IsWriteMem(sel: UInt): Bool = {
    val select = sel === LocalDataFlowControl.accumulatorToMemory
    select
  }

  /* memory -> array.weight || memory -> array.input */
  val memReadStream = new BetsyStreamDemux(gen, 3)
  memReadStream.io.InStream << io.memoryDataFlow.memOut
  memReadStream.io.OutStreams(0) >> io.arrayDataFlow.weight
  memReadStream.io.OutStreams(1) >> io.arrayDataFlow.input
  memReadStream.io.sel.valid := io.control.valid && IsMemRead(io.control.sel)
  memReadStream.io.sel.payload.clearAll()
  /* array to Acc || memory -> Acc*/
  val writeAccStream = new BetsyStreamMux(gen, 2)
  writeAccStream.io.InStreams(0) << io.arrayDataFlow.output
  writeAccStream.io.InStreams(1) << memReadStream.io.OutStreams(2)
  writeAccStream.io.OutStream >> io.accumulatorDataFlow.accIn
  writeAccStream.io.sel.valid := io.control.valid && IsWriteAcc(io.control.sel)
  writeAccStream.io.sel.payload.clearAll()
  /* acc to memory */
  val writeMemStream = new BetsyStreamMux(gen, 2)
  writeMemStream.io.InStreams(1) << io.accumulatorDataFlow.accOut
  writeMemStream.io.InStreams(0).valid := False
  writeMemStream.io.InStreams(0).payload.clearAll()
  writeMemStream.io.OutStream >> io.memoryDataFlow.memIn
  writeMemStream.io.sel.valid := io.control.valid && IsWriteMem(io.control.sel)
  writeMemStream.io.sel.payload.clearAll()

  /* control the read and write size*/
  val memReadSizeHandler = new SizeHandler(new LocalDataFlowControlWithSize(arch.localDepth),
    new DataFlowSel(LocalDataFlowControl.locaDataFlowNums),
    depth = arch.localDepth)

  val writeAccSizeHandler = new SizeHandler(new LocalDataFlowControlWithSize(arch.localDepth),
    new DataFlowSel(LocalDataFlowControl.locaDataFlowNums),
    depth = arch.localDepth)

  val writeMemSizeHandler = new SizeHandler(new LocalDataFlowControlWithSize(arch.localDepth),
    new DataFlowSel(LocalDataFlowControl.locaDataFlowNums),
    depth = arch.localDepth)

  val logic = new Area {

    val enqueue1 = MultiEnqControl(2)
    val enqueue2 = MultiEnqControl(2)

    memReadSizeHandler.io.into.valid := io.control.valid && IsMemRead(io.control.sel)
    memReadSizeHandler.io.into.payload := io.control.payload
    memReadSizeHandler.io.output.ready := memReadStream.io.sel.ready

    writeAccSizeHandler.io.into.payload := io.control.payload
    writeAccSizeHandler.io.into.valid := io.control.valid && IsWriteAcc(io.control.sel)
    writeAccSizeHandler.io.output.ready := writeAccStream.io.sel.ready

    writeMemSizeHandler.io.into.payload := io.control.payload
    writeMemSizeHandler.io.into.valid := io.control.valid && IsWriteMem(io.control.sel)
    writeMemSizeHandler.io.output.ready := writeMemStream.io.sel.ready

    when(io.control.sel === LocalDataFlowControl.memoryToArrayWeight) {
      memReadStream.io.sel.payload := U(0).resized
      io.control.ready := memReadSizeHandler.io.into.ready
    }.elsewhen(io.control.sel === LocalDataFlowControl.memoryToArrayToAcc) {
      memReadStream.io.sel.payload := U(1, 2 bits)
      writeAccStream.io.sel.payload := U(0, 1 bits)
      io.control.ready := enqueue1.Readyenqueue2(io.control.valid, memReadSizeHandler.io.into.ready, writeAccSizeHandler.io.into.ready)
    }.elsewhen(io.control.sel === LocalDataFlowControl.arrayToAcc) {
      writeAccStream.io.sel.payload := U(0).resized
      io.control.ready := writeAccSizeHandler.io.into.ready
    }.elsewhen(io.control.sel === LocalDataFlowControl.accumulatorToMemory) {
      writeMemStream.io.sel.payload := U(1).resized
      io.control.ready := writeMemSizeHandler.io.into.ready
    }.elsewhen(io.control.sel === LocalDataFlowControl.memoryToAccumulator) {
      memReadStream.io.sel.payload := U(2, 2 bits)
      writeAccStream.io.sel.payload := U(1, 1 bits)
      io.control.ready := enqueue2.Readyenqueue2(io.control.valid,
        memReadSizeHandler.io.into.ready,
        writeAccSizeHandler.io.into.ready)
    }.otherwise {
      io.control.ready := True
    }
  }
}
