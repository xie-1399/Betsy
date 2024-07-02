package Betsy

import spinal.core._
import spinal.lib._
import Betsy.Until._
import BetsyLibs._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94      **
 ** You should have received a copy of the MIT License along with this library **
 ** the data out is get the memory data to the dram **
 ** the data in is get the dram data to the memory **
 ** the kind show is data in or data out and which port to in out **
 ** Test Status : PASS :)         Version:0.2 **
 */

case class HostData[T <: Data](val gen:HardType[T]) extends Bundle with IMasterSlave {
  val dataIn = Stream(gen())
  val dataOut = Stream(gen())
  override def asMaster(): Unit = {
    master(dataOut)
    slave(dataIn)
  }
}


class HostRouter[T <: Data](val gen:HardType[T]) extends BetsyModule{
  /* the stream mux lib looks like also work (update it later...) */
  val io = new Bundle{
    val control = slave(Stream(new HostDataFlowControl())) /* with kind control */
    val dram0 = master (HostData(gen))
    val dram1 = master (HostData(gen))
    val mem = slave (HostData(gen))
  }

  val isDataIn = HostDataFlowControl.isDataIn(io.control.kind)
  val isDataOut = HostDataFlowControl.isDataOut(io.control.kind)

  /* dram 0 and dram 1 -> mem.dataIn*/
  val memInSel = BetsyStreamMux(
    io.dram0.dataIn,
    io.dram1.dataIn,
    io.mem.dataIn,
    io.control.valid && isDataIn,
    io.control.kind.msb.asUInt)

  /* memory.dataOut -> dram 0 and dram 1*/
  val dramOutSel = BetsyStreamDemux(
    io.mem.dataOut,
    io.dram0.dataOut,
    io.dram1.dataOut,
    io.control.valid && isDataOut,
    io.control.kind.msb.asUInt
  )
  io.control.ready := (isDataIn && memInSel) || (isDataOut && dramOutSel)
}


