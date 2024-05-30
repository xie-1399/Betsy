package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94      **
 ** You should have received a copy of the MIT License along with this library **
 ** the local router control the data move between Accumulator/ Array / Scratch Pad**
 */

//Todo fix all
import Betsy.Until._
import BetsyLibs.{BetsyStreamDemux, BetsyStreamMux}
import spinal.core._
import spinal.lib._

/* the dataflow only controls the data move process */
case class MemoryDataFlow[T <: Data](gen:HardType[T]) extends Bundle with IMasterSlave {
  val memIn = Stream(gen())
  val memOut = Stream(gen())
  override def asMaster(): Unit = {
    master(memOut)
    slave(memIn)
  }
}

case class AccumulatorDataFlow[T <: Data](gen:HardType[T]) extends Bundle with IMasterSlave {
  val accIn = Stream(gen())
  val accOut = Stream(gen())
  override def asMaster(): Unit = {
    master(accOut)
    slave(accIn)
  }
}

case class ArrayDataFlow[T <: Data](gen:HardType[T]) extends Bundle with IMasterSlave{
  val weight = Stream(gen())
  val input = Stream(gen())
  val output = Stream(gen())

  override def asMaster(): Unit = {
    master(output)
    slave(input,weight)
  }
}


class LocalRouter[T <: Data](gen:HardType[T],arch:Architecture) extends BetsyModule{

  val io = new Bundle{
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
    val select = sel === LocalDataFlowControl.arrayToAcc
    select
  }

  def IsWriteMem(sel:UInt): Bool = {
    val select = sel === LocalDataFlowControl.accumulatorToMemory
    select
  }

  /* memory -> array.weight || memory -> array.input */
  val memReadStreams = new BetsyStreamDemux(gen,3)
  memReadStreams.io.InStream << io.memoryDataFlow.memOut
  memReadStreams.io.OutStreams(0) >> io.arrayDataFlow.weight
  memReadStreams.io.OutStreams(1) >> io.arrayDataFlow.input
  /* array to Acc || memory -> Acc*/
  val writeAccStream = new BetsyStreamMux(gen,2)
  writeAccStream.io.InStreams(0) << io.arrayDataFlow.output
  writeAccStream.io.InStreams(1) << memReadStreams.io.OutStreams(2)
  writeAccStream.io.OutStream >> io.accumulatorDataFlow.accIn
  /* acc to memory connect */
  val writeMemStream = new BetsyStreamMux(gen,2)
  writeMemStream.io.InStreams(0) << io.accumulatorDataFlow.accOut
  writeMemStream.io.InStreams(1).valid := False
  writeMemStream.io.InStreams(1).payload := zero(gen())
  writeMemStream.io.OutStream >> io.memoryDataFlow.memIn

  /* control the read and write size*/
  val memReadSizeHandler = new SizeHandler(new LocalDataFlowControlWithSize(arch.localDepth),new DataFlowSel(3),depth = arch.localDepth)
  memReadSizeHandler.io.into.payload := io.control.payload
  memReadSizeHandler.io.into.valid := io.control.valid && IsMemRead(io.control.kind)

  memReadStreams.io.sel.valid := memReadSizeHandler.io.output.valid
  memReadStreams.io.sel.payload := memReadSizeHandler.io.output.sel

  val writeAccSizeHandler = new SizeHandler(new LocalDataFlowControlWithSize(arch.localDepth),new DataFlowSel(2),depth = arch.localDepth)
  writeAccSizeHandler.io.into.payload := io.control.payload
  writeAccSizeHandler.io.into.valid := io.control.valid && IsWriteAcc(io.control.kind)
  writeAccStream.io.sel.valid := writeAccSizeHandler.io.output.valid
  writeAccStream.io.sel.payload := writeAccSizeHandler.io.output.sel

  val writeMemSizeHandler = new SizeHandler(new LocalDataFlowControlWithSize(arch.localDepth),new DataFlowSel(2),depth = arch.localDepth)
  writeMemSizeHandler.io.into.payload := io.control.payload
  writeMemSizeHandler.io.into.valid := io.control.valid && IsWriteMem(io.control.kind)
  writeMemStream.io.sel.valid := writeMemSizeHandler.io.output.valid
  writeMemStream.io.sel.payload := writeMemSizeHandler.io.output.sel
  io.control.ready := True // Todo
}


object LocalRouter extends App {
  val arch = Architecture()
  SpinalSystemVerilog(new LocalRouter(Bits(16 bits),arch))
}
