package Betsy

import Betsy.Until._
import spinal.core._
import spinal.lib._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Test Status : PASS :)         Version:0.1  **
 */

case class BetsyStreamPass[T <: Data](data: HardType[T]) extends Bundle with IMasterSlave {
  /* operation between in and out stream */
  val dataIn = Stream(data)
  val dataOut = Stream(data)

  override def asMaster(): Unit = {
    slave(dataIn)
    master(dataOut)
  }
}

class BetsyStreamMux[T<:Data](val gen:HardType[T],val num:Int) extends BetsyModule{

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

class BetsyStreamDemux[T <: Data](val gen:HardType[T],val num:Int) extends BetsyModule{
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

class BetsyStreamDelay[T<:Data](gen:HardType[T],cycles:Int) extends BetsyModule{
  val io = new Bundle{
    val input = slave(Stream(gen))
    val output = master(Stream(gen))
    val delayed = out(gen())
  }
  val delays = Delay(io.input.payload,cycles,io.input.valid && io.output.ready,init = zero(gen()))
  io.delayed := delays
  io.output <> io.input
}


object BetsyStreamUsage extends App{
  val BetsyStreamMux = SpinalSystemVerilog(new BetsyStreamMux(Bits(3 bits),4))
  val BetsyStreamDeMux = SpinalSystemVerilog(new BetsyStreamDemux(Bits(3 bits),4))
  val BetsyStreamDelay = SpinalSystemVerilog(new BetsyStreamDelay(Bits(3 bits),4))
}