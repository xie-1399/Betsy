package BetsyLibs

/*
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/11      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Todo be tested...
*/

import Betsy.Until._
import spinal.core._
import spinal.lib._

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

  def block() = {
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

  def enqueue3[T <: Data, S <: Data, R <: Data](valid: Bool, out0: Stream[T], out0Bits: T, out1: Stream[S], out1Bits: S,out2: Stream[R], out2Bits: R): Bool = {
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

  def enqueue5[T <: Data, S <: Data, R <: Data, Q <: Data, P <: Data](valid: Bool, out0: Stream[T], out0Bits: T, out1: Stream[S], out1Bits: S, out2: Stream[R],
                                                                      out2Bits: R, out3: Stream[Q], out3Bits: Q,out4: Stream[P], out4Bits: P): Bool = {
    io.into.valid := valid
    out0 <> io.output(0).toStream(out0Bits)
    out1 <> io.output(1).toStream(out1Bits)
    out2 <> io.output(2).toStream(out2Bits)
    out3 <> io.output(3).toStream(out3Bits)
    out4 <> io.output(4).toStream(out4Bits)
    io.into.ready
  }
}

/* demo using the Control to create 4 multi queue*/
class MyMultiQueue(val size:Int = 4) extends BetsyModule{
  
  val io = new Bundle{
    val into = slave(BetsyReadyValid())
    val payload0 = in Bits(3 bits)
    val payload1 = in UInt(6 bits)
    val payload2 = in SInt(9 bits)
    val payload3 = in Bits(12 bits)

    val stream0 = master(Stream(Bits(3 bits)))
    val stream1 = master(Stream(UInt(6 bits)))
    val stream2 = master(Stream(SInt(9 bits)))
    val stream3 = master(Stream(Bits(12 bits)))
  }
  
  val multiEnqControl = new MultiEnqControl(size)
  val allIn = multiEnqControl.enqueue4(io.into.valid,io.stream0,io.payload0,io.stream1,io.payload1,io.stream2,io.payload2,io.stream3,io.payload3)
  io.into.ready := allIn
}


object MyMultiQueue extends App{
  SpinalSystemVerilog(new MyMultiQueue())
}


object MultiEnqControl extends App{
  import spinal.core.sim._

  SIMCFG().compile{
    val dut = new MultiEnqControl(4)
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      simSuccess()

  }
}