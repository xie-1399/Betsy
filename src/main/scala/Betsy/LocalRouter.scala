package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94      **
 ** You should have received a copy of the MIT License along with this library **
 ** the local router control the data move between Accumulator/ Array / Scratch Pad**
 */

import Betsy.Until._
import BetsyLibs.BetsyStreamDemux
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

  /* memory -> Acc || memory -> array.weight || memory -> array.input */
  val memReadStreams = new BetsyStreamDemux(gen,3)
  memReadStreams.io.InStream << io.memoryDataFlow.memOut
  memReadStreams.io.sel.payload := io.control.kind

  memReadStreams.io.OutStreams(0) >> io.arrayDataFlow.weight
  memReadStreams.io.OutStreams(1) >> io.arrayDataFlow.input
}
