package BetsyUntils

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the Pipeline can delay pass the signals out and with  history goes **
 ** Test Status : PASS :)         Version:0.1                 **
 */

import spinal.core._
import spinal.lib._
import Betsy.Until._


class Pipeline[T<:Data](gen:HardType[T],latency:Int) extends BetsyModule {
  require(latency > 0,"Latency value error!!!")

  val io = new Bundle{
    val input = slave(Stream(gen))
    val output = master(Stream(gen))
    val busy = out Bool()
  }
  if(latency < 1){
    io.output.connectFrom(io.input)
    io.busy := io.input.valid
  }
  else{
    /* latency goes and delay */
    val initStream = Stream(gen())
    initStream.setIdle()
    val stages = History(io.input,latency,init = initStream,when = io.output.ready)
    val valids = stages.map(_.valid)

    io.output.connectFrom(stages(latency - 1))
    io.busy := io.input.valid || valids.reduce(_ || _)

    io.input.ready := io.output.ready
  }
}

object Pipeline{
  def apply[T <: Data](input: Stream[T], latency: Int): Stream[T] = {
    val p = new Pipeline(cloneOf(input.payload), latency)
    p.io.input <> input
    p.io.output
  }
}