package BetsyUntils

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the multi tail queue can receive N pushes(0<=N<=max) and pop one by one **
 ** Test Status : PASS :)      Version:0.1  **
 */

import spinal.core._
import spinal.lib._
import Betsy.Until._

case class Tail[T <:Data](gen:HardType[T],entries:Int,maxpush:Int) extends Bundle with IMasterSlave {
  val push = UInt(log2Up(maxpush) + 1 bits)
  val payloads = Vec(gen,maxpush)
  val up = UInt(log2Up(maxpush) + 1 bits) /* push up nums data */
  override def asMaster(): Unit = {
    out(push,payloads)
    in(up)
  }
}

class MultiTailedQueue[T <:Data](gen:HardType[T],entries:Int,maxpush:Int) extends BetsyModule {
  require(maxpush >= 1,"max push must great than 1!!!")

  val io = new Bundle{
    val enq = slave(Tail(gen,entries,maxpush))
    val deq = master(Stream(gen))
  }

  /* here push some values in and get the one stream out queue */
  val tailQueue = Mem(gen,entries)
  val avail = Reg(UInt(log2Up(entries) + 1 bits)).init(entries)
  val waddr = Reg(UInt(log2Up(entries) bits)).init(0)
  val raddr = Reg(UInt(log2Up(entries) bits)).init(0)

  /* write the tail queue with push data*/
  for(i <- 0 until maxpush){
    tailQueue.write(waddr + i,io.enq.payloads(i),enable = io.enq.push > i)
  }

  /* test the wrapping add function */
  waddr := wrappingAdd(waddr,io.enq.push,entries)
  raddr := wrappingAdd(raddr,io.deq.fire.asUInt,entries)

  avail := avail - io.enq.push + io.deq.fire.asUInt
  io.enq.up := Mux(avail < maxpush,avail.resized,U(maxpush))
  io.deq.valid := avail < entries
  io.deq.payload := tailQueue.readAsync(raddr)
  assert(io.enq.push <= io.enq.up && io.enq.up <= avail)
}

object MultiTailedQueue {
  def apply[T <: Data](gen: HardType[T], entries: Int, maxpush: Int) = {
    new MultiTailedQueue(gen, entries, maxpush)
  }
}
