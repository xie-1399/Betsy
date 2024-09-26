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
    this.ready := stream.ready
    stream.payload := payload
    stream
  }

  def noenq() = {this.valid := False}
  def nodeq() = {this.ready := False}
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

object BetsyStreamMux{
  /* two streams */
  def apply[T <: Data](in0: Stream[T], in1: Stream[T], out: Stream[T]): Stream[UInt] = {
    val mux = new BetsyStreamMux(cloneOf(in0.payload), 2)
    mux.io.InStreams(0) << in0
    mux.io.InStreams(1) << in1
    mux.io.OutStream >> out
    mux.io.sel
  }

  def apply[T <: Data](in0: Stream[T], in1: Stream[T], out: Stream[T], valid: Bool, kind: UInt): Bool = {
    val mux = new BetsyStreamMux(cloneOf(in0.payload), 2)
    mux.io.InStreams(0) << in0
    mux.io.InStreams(1) << in1
    mux.io.OutStream >> out
    mux.io.sel.valid := valid
    mux.io.sel.payload := kind.resized
    mux.io.sel.ready
  }
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

object BetsyStreamDemux{
  def apply[T <: Data](in: Stream[T], out0: Stream[T], out1: Stream[T]): Stream[UInt] = {
    val demux = new BetsyStreamDemux(cloneOf(in.payload), 2)
    demux.io.InStream << in
    demux.io.OutStreams(0) >> out0
    demux.io.OutStreams(1) >> out1
    demux.io.sel
  }

  def apply[T <: Data](in: Stream[T], out0: Stream[T], out1: Stream[T], valid: Bool, kind: UInt): Bool =  {
    val demux = new BetsyStreamDemux(cloneOf(in.payload), 2)
    demux.io.InStream << in
    demux.io.OutStreams(0) >> out0
    demux.io.OutStreams(1) >> out1
    demux.io.sel.valid := valid
    demux.io.sel.payload := kind.resized
    demux.io.sel.ready
  }
}

class Sink[T <: Data](gen:HardType[T]) extends BetsyModule{
  val io = new Bundle{
    val into = slave Stream gen
  }
  io.into.ready := True /* when the stream coming always fire */
}


class BetsyStreamDelay[T<:Data](gen:HardType[T],cycles:Int) extends BetsyModule{
  val io = new Bundle{
    val input = slave(Stream(gen))
    val output = master(Stream(gen))
    val delayed = out(gen())
  }
  val delays = Delay(io.input.payload,cycles,io.input.valid && io.output.ready,init = constConvert(gen(),0))
  io.delayed := delays
  io.output <> io.input
}