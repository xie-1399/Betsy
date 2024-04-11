package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

import Betsy.Until.BetsyModule
import spinal.core._
import spinal.lib._

trait TagQueueTag{
  def garbage(dummy:Int = 0):Unit
}

/* using the simulation test bundle */
case class tagBundle() extends Bundle with TagQueueTag {
  val id = UInt(3 bits)
  val tag = UInt(3 bits)
  override def garbage(dummy: Int = 0): Unit = {
    this.id := U(dummy).resized
    this.tag := U(dummy).resized
  }
}

class TagQueue[T <: Data with TagQueueTag](tag:HardType[T],entries:Int) extends BetsyModule{
  val io = new Bundle{
    val enq = slave(Stream(tag))
    val deq = master(Stream(tag))
    val all = out(Vec(tag,entries))
  }

  val Tags = Reg(Vec(tag,entries))
  // Tags.foreach(_.garbage())

  val len = Reg(UInt(log2Up(entries + 1) bits)).init(0)
  val raddr = Reg(UInt(log2Up(entries) bits)).init(0)
  val waddr = Reg(UInt(log2Up(entries) bits)).init(0)
  val empty = len === 0
  val full = len === entries

  when(io.enq.fire){
    Tags(waddr) := io.enq.payload
    waddr := waddr + 1
  }
  when(io.deq.fire){
    Tags(raddr).garbage()
    raddr := raddr + 1
  }

  when(io.enq.fire && !io.deq.fire){
    len := len + 1
  }.elsewhen(!io.enq.fire && io.deq.fire){
    len := len - 1
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.payload := Tags(raddr)
  io.all := Tags
}

object TagQueue extends App{
  SpinalSystemVerilog(new TagQueue(tagBundle(),4))
}