package Sloan

import Sloan.Until._
import spinal.lib._
import spinal.core._

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Test Status : PASS :)         Version:0.1  **
 */

case class SloanStreamPass[T <: Data](val data: HardType[T]) extends Bundle with IMasterSlave {
  /* operation between in and out stream */
  val dataIn = Stream(data)
  val dataOut = Stream(data)

  override def asMaster(): Unit = {
    slave(dataIn)
    master(dataOut)
  }
}

class SloanStreamMux[T<:Data](val gen:HardType[T],val num:Int) extends SloanModule{

  val io = new Bundle{
    val InStreams = Vec(slave(Stream(gen)),num)
    val sel = slave(Stream(UInt(log2Up(num) bits)))
    val OutStream = master(Stream(gen))
  }

  io.InStreams.foreach(_.ready := False)
  // val inputs = Vec(Stream(gen),num)
  val select = io.InStreams(io.sel.payload)
  select.ready := io.OutStream.ready && io.sel.valid

  io.OutStream.payload := select.payload
  io.OutStream.valid := io.sel.valid && select.valid
  io.sel.ready := io.OutStream.ready && select.valid
}

class SloanStreamDemux[T <: Data](val gen:HardType[T],val num:Int) extends SloanModule{
  val io = new Bundle {
    val InStream = slave(Stream(gen))
    val sel = slave(Stream(UInt(log2Up(num) bits)))
    val OutStreams = Vec(master(Stream(gen)), num)
  }
  io.OutStreams.foreach{ /* init out */
    s =>
      s.valid := False
      s.payload.clearAll()
  }

  val select = io.OutStreams(io.sel.payload)
  select.payload := io.InStream.payload
  select.valid := io.sel.valid && io.InStream.valid
  io.sel.ready := io.InStream.valid && select.ready
  io.InStream.ready := io.sel.valid && select.ready
}

object SloanStreamUsage extends App{
  val sloanStreamMux = SpinalSystemVerilog(new SloanStreamMux(Bits(3 bits),4))
  val sloanStreamDeMux = SpinalSystemVerilog(new SloanStreamDemux(Bits(3 bits),4))
}