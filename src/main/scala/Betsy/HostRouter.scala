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
 ** Test Status : PASS :)         Version:0.1 **
 */

class HostRouter[T <: Data](val gen:HardType[T]) extends BetsyModule{
  /* the stream mux lib looks like also work (update it later...) */
  val io = new Bundle{
    val control = slave(Stream(new HostDataFlowControl())) /* with kind control */
    val dram0 = master (BetsyStreamPass(gen))
    val dram1 = master (BetsyStreamPass(gen))
    val mem = slave (BetsyStreamPass(gen))
  }

  val BetsyStreamMux = new BetsyStreamMux(gen,2)
  val BetsyStreamDeMux = new BetsyStreamDemux(gen,2)
  val isDataIn = HostDataFlowControl.isDataIn(io.control.kind)
  val isDataOut = HostDataFlowControl.isDataOut(io.control.kind)

  /* dram 0 and dram 1 -> mem.dataIn*/
  BetsyStreamMux.io.InStreams(0) <>io.dram0.dataIn
  BetsyStreamMux.io.InStreams(1) <> io.dram1.dataIn
  io.mem.dataIn <> BetsyStreamMux.io.OutStream
  BetsyStreamMux.io.sel.valid := io.control.valid && isDataIn
  BetsyStreamMux.io.sel.payload.assignFromBits(io.control.kind(1).asBits)

  /* memory.dataOut -> dram 0 and dram 1*/
  BetsyStreamDeMux.io.InStream <> io.mem.dataOut
  BetsyStreamDeMux.io.OutStreams(0) <> io.dram0.dataOut
  BetsyStreamDeMux.io.OutStreams(1) <> io.dram1.dataOut
  BetsyStreamDeMux.io.sel.valid := io.control.valid && isDataOut
  BetsyStreamDeMux.io.sel.payload.assignFromBits(io.control.kind(1).asBits)

  io.control.ready := (isDataIn && BetsyStreamMux.io.sel.ready) || (isDataOut && BetsyStreamDeMux.io.sel.ready)
}


