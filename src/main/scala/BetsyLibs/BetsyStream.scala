package BetsyLibs

import Betsy.Until._
import spinal.core._
import spinal.lib._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** some stream modules for the Betsy  **
 ** Test Status : PASS :)         Version:0.1  **
 */

case class BetsyReadyValid() extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = Bool()

  override def asMaster(): Unit = {
    out(valid)
    in(ready)
  }

  def fire(): Bool = this.valid && this.ready

  def toStream[T <: Data](payload: T): Stream[T] = {
    val stream = Stream(cloneOf(payload))
    stream.valid := this.valid
    stream.ready := this.ready
    stream.payload := payload
    stream
  }
}

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
  /* the select stream is Drive by the sel stream*/
  val io = new Bundle{
    val InStreams = Vec(slave(Stream(gen)),num)
    val sel = slave(Stream(UInt(log2Up(num) bits)))
    val OutStream = master(Stream(gen))
  }

  io.InStreams.foreach(_.ready := False)
  val selectStream = io.InStreams(io.sel.payload)
  selectStream.ready := io.sel.valid && io.OutStream.ready
  io.sel.ready := selectStream.valid && io.OutStream.ready

  io.OutStream.payload := selectStream.payload
  io.OutStream.valid := selectStream.valid && io.sel.valid
}

class BetsyStreamDemux[T <: Data](val gen:HardType[T],val num:Int) extends BetsyModule{
  /* from one stream with Multi streams out(select by the sel stream ) */
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
  val selectStream = io.OutStreams(io.sel.payload)
  selectStream.valid := io.sel.valid && io.InStream.valid
  selectStream.payload := io.InStream.payload
  io.InStream.ready := io.sel.valid && selectStream.ready
  io.sel.ready := io.InStream.valid && selectStream.ready
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