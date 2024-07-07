package BetsyLibs

 /*
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/11      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Test Status : PASS :)         Version:0.1  **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._
import spinal.lib.sim.StreamReadyRandomizer
import scala.util.Random

/* whether all signals are enqueued or not */
class MultiEnqControl(size:Int) extends BetsyModule{
  require(size > 0,"multiEnq size should > 0 !!!")

  val io = new Bundle{
    val into = slave(BetsyReadyValid())
    val output = Vec(master(BetsyReadyValid()),size)
  }

  val enqs = Vec(RegInit(False),size)
  val allEnqueue = io.output.map(_.ready).zip(enqs).map{ /* all data is into the queue or all is ready */
    case (ready,enq) => ready || enq
  }.reduceBalancedTree(_ && _)

  for(i <- 0 until size){
    io.output(i).valid := io.into.valid && !enqs(i)
    when(allEnqueue){
      enqs(i).clear()
    }.otherwise{
      when(!enqs(i)){
        enqs(i) := io.output(i).fire()
      }
    }
  }
  io.into.ready := allEnqueue

  def block(): Unit = {
    io.into.valid := False
    io.output.foreach(_.ready := False)
  }


  def enqueue[T <: Data](valid: Bool, out: Stream[T], payload: T): Bool = {
    io.into.valid := valid
    out <> io.output(0).toStream(payload)
    io.into.ready
  }

  def enqueue2[T <: Data, S <: Data](valid: Bool, out0: Stream[T], out0Bits: T, out1: Stream[S], out1Bits: S) : Bool = {
    io.into.valid := valid
    out0 <> io.output(0).toStream(out0Bits)
    out1 <> io.output(1).toStream(out1Bits)
    io.into.ready
  }

  def enqueue3[T <: Data, S <: Data, R <: Data](valid: Bool, out0: Stream[T], out0Bits: T, out1: Stream[S],
                                                out1Bits: S,out2: Stream[R], out2Bits: R): Bool = {
    io.into.valid := valid
    out0 <> io.output(0).toStream(out0Bits)
    out1 <> io.output(1).toStream(out1Bits)
    out2 <> io.output(2).toStream(out2Bits)
    io.into.ready
  }

  def enqueue4[T <: Data, S <: Data, R <: Data, Q <: Data](valid: Bool, out0: Stream[T], out0Bits: T, out1: Stream[S], out1Bits: S, out2: Stream[R],
                                                out2Bits: R,out3: Stream[Q], out3Bits: Q): Bool = {
    io.into.valid := valid
    out0 <> io.output(0).toStream(out0Bits)
    out1 <> io.output(1).toStream(out1Bits)
    out2 <> io.output(2).toStream(out2Bits)
    out3 <> io.output(3).toStream(out3Bits)
    io.into.ready
  }

  /* two ready enqueue */
  def Readyenqueue2(valid: Bool, ready1: Bool, ready2: Bool): Bool = {
    io.into.valid := valid
    io.output(0).ready := ready1
    io.output(1).ready := ready2
    io.into.ready
  }

  /* three ready enqueue */
  def Readyenqueue3(valid: Bool, ready1: Bool, ready2: Bool, ready3:Bool): Bool = {
    io.into.valid := valid
    io.output(0).ready := ready1
    io.output(1).ready := ready2
    io.output(2).ready := ready3
    io.into.ready
  }

  /* four ready enqueue */
  def Readyenqueue4(valid: Bool, ready1: Bool, ready2: Bool, ready3: Bool, ready4:Bool): Bool = {
    io.into.valid := valid
    io.output(0).ready := ready1
    io.output(1).ready := ready2
    io.output(2).ready := ready3
    io.output(3).ready := ready4
    io.into.ready
  }
}

object MultiEnqControl{
  def apply(size: Int): MultiEnqControl = {
    val multiQueue = new MultiEnqControl(size)
    multiQueue.block()
    multiQueue
  }
}