package Sloan

import Sloan.Until._
import spinal.core._
import spinal.lib._

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94      **
 ** You should have received a copy of the MIT License along with this library **
 ** the data out is get the memory data to the dram **
 ** the data in is get the dram data to the memory **
 ** the kind show is data in or data out and which port to in out **
 ** Todo to be tested !!! **
 */

case class HostDataFlowControl() extends Bundle{
  val kind = UInt(2 bits)
}

object HostDataFlowControl{
  def In0 = U(0x0) /* dram 0 -> to the memory*/
  def Out0 = U(0x1) /* memory -> to the dram0 */
  def In1 = U(0x2) /* dram 1 -> to the memory */
  def Out1 = U(0x3) /* memory -> to the dram1 */
  def isDataIn(kind: UInt): Bool = {
    kind === In0 || kind === In1
  }

  def isDataOut(kind: UInt): Bool = {
    kind === Out0 || kind === Out1
  }
  def apply(kind:UInt):HostDataFlowControl = {
    val w = HostDataFlowControl()
    w.kind := kind
    w
  }
}

class HostRouter[T <: Data](val gen:HardType[T]) extends SloanModule{
  /* the stream mux lib looks like also work (update it later...) */
  val io = new Bundle{
    val control = slave(Stream(new HostDataFlowControl())) /* with kind control */
    val dram0 = master (SloanStreamPass(gen))
    val dram1 = master (SloanStreamPass(gen))
    val mem = slave (SloanStreamPass(gen))
  }

  val sloanStreamMux = new SloanStreamMux(gen,2)
  val sloanStreamDeMux = new SloanStreamDemux(gen,2)
  val isDataIn = HostDataFlowControl.isDataIn(io.control.kind)
  val isDataOut = HostDataFlowControl.isDataOut(io.control.kind)

  /* dram 0 and dram 1 -> mem.dataIn*/
  sloanStreamMux.io.InStreams(0) <>io.dram0.dataIn
  sloanStreamMux.io.InStreams(1) <> io.dram1.dataIn
  io.mem.dataIn <> sloanStreamMux.io.OutStream
  sloanStreamMux.io.sel.valid := io.control.valid && isDataIn
  sloanStreamMux.io.sel.payload.assignFromBits(io.control.kind(1).asBits)

  /* memory.dataOut -> dram 0 and dram 1*/
  sloanStreamDeMux.io.InStream <> io.mem.dataOut
  sloanStreamDeMux.io.OutStreams(0) <> io.dram0.dataOut
  sloanStreamDeMux.io.OutStreams(1) <> io.dram1.dataOut
  sloanStreamDeMux.io.sel.valid := io.control.valid && isDataOut
  sloanStreamDeMux.io.sel.payload.assignFromBits(io.control.kind(1).asBits)

  io.control.ready := (isDataIn && sloanStreamMux.io.sel.ready) || (isDataOut && sloanStreamDeMux.io.sel.ready)
}


object HostRouter extends App{
  SpinalSystemVerilog(new HostRouter(Bits(16 bits)))
}
