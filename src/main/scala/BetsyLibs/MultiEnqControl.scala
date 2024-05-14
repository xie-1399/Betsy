package BetsyLibs

/*
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/11      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 **
*/

import Betsy.Until._
import spinal.core._
import spinal.lib._

class MultiEnqControl(size:Int) extends BetsyModule{
  require(size > 0,"multiEnq size should > 0 !!!")

  val io = new Bundle{
    val in = slave(BetsyReadyValid())
    val out = Vec(master(BetsyReadyValid()),size)
  }

  val enqs = Vec(RegInit(False),size)
  val allEnqueue = io.out.map(_.ready).zip(enqs).map{ /* all data is into the queue or all is ready */
    case (ready,enq) => ready || enq
  }.reduceBalancedTree(_ && _)

  for(i <- 0 until size){
    io.out(i).valid := io.in.valid && !enqs(i)
    when(allEnqueue){
      enqs(i).clear()
    }.otherwise{
      when(!enqs(i)){
        enqs(i) := io.out(i).fire()
      }
    }
  }
  io.in.ready := allEnqueue

  def block() = {
    io.in.valid := False
    io.out.foreach(_.ready := False)
  }

  def enqueue[T <: Data](valid: Bool, out: Stream[T], payload: T): Bool = {
    io.in.valid := valid
    out <> io.out(0).toStream(payload)
    io.in.ready
  }
}


object MultiEnqControl extends App{
  SpinalSystemVerilog(new MultiEnqControl(4))
}