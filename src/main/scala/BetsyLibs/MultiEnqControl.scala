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

  def enqueue5[T <: Data, S <: Data, R <: Data, Q <: Data, P <: Data](valid: Bool, out0: Stream[T], out0Bits: T, out1: Stream[S], out1Bits: S, out2: Stream[R], out2Bits: R, out3: Stream[Q], out3Bits: Q,out4: Stream[P], out4Bits: P): Bool = {
    io.into.valid := valid
    out0 <> io.output(0).toStream(out0Bits)
    out1 <> io.output(1).toStream(out1Bits)
    out2 <> io.output(2).toStream(out2Bits)
    out3 <> io.output(3).toStream(out3Bits)
    out4 <> io.output(4).toStream(out4Bits)
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
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new MyMultiQueue()
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      StreamReadyRandomizer(dut.io.stream0, dut.clockDomain)
      StreamReadyRandomizer(dut.io.stream1, dut.clockDomain)
      StreamReadyRandomizer(dut.io.stream2, dut.clockDomain)
      StreamReadyRandomizer(dut.io.stream3, dut.clockDomain)
      dut.io.into.valid #= false
      dut.io.payload0.randomize()
      dut.io.payload1.randomize()
      dut.io.payload2.randomize()
      dut.io.payload3.randomize()
      dut.clockDomain.waitSampling()
      def testCase = 1024
      var num = 0
      val data = Array.fill(testCase){Array.fill(4){Random.nextInt(8)}}

      while (num < testCase){
        dut.io.into.valid.randomize()
        dut.io.payload0 #= data(num)(0)
        dut.io.payload1 #= data(num)(1)
        dut.io.payload2 #= data(num)(2)
        dut.io.payload3 #= data(num)(3)
        dut.clockDomain.waitSampling()
        if(dut.io.into.valid.toBoolean && dut.io.into.ready.toBoolean){
          assert(dut.io.stream0.payload.toInt == data(num)(0))
          assert(dut.io.stream1.payload.toInt == data(num)(1))
          assert(dut.io.stream2.payload.toInt == data(num)(2))
          assert(dut.io.stream3.payload.toInt == data(num)(3))
          num += 1
        }
      }
      simSuccess()
  }
}