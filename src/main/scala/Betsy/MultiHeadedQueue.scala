package Betsy
/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the MultiHead receive from one master and send to heads(pop decides the real num in fact)
 ** Test Status : PASS :)         Version:0.1                 **
 */
import spinal.core._
import spinal.lib._
import Betsy.Until._
import scala.util.Random

case class Head[T <: Data](gen:HardType[T],entries:Int,heads:Int,maxpop:Int = 2) extends Bundle with IMasterSlave {
  val valids = Vec(Bool(),heads)
  val payloads = Vec(gen,heads)
  val pop = UInt(log2Up(entries min maxpop) bits) //Todo
  override def asMaster(): Unit = {
    out(valids,payloads)
    in(pop)
  }
}

class MultiHeadedQueue[T <: Data](gen:HardType[T],entries:Int,heads:Int,maxpop:Int = 2) extends BetsyModule{

  assert(heads >= 1, "MultiHeadedQueue's Head is illegal")

  val io = new Bundle{
    val enq = slave(Stream(gen))
    val deq = master(Head(gen,entries,heads,maxpop))
    val len = out(UInt(log2Up(entries + 1) bits))
  }

  val len = Reg(UInt(log2Up(entries + 1)  bits) ).init(0)
  val waddr = Reg(UInt(log2Up(entries) max 1 bits) ).init(0)
  val raddr = Reg(UInt(log2Up(entries) max 1 bits) ).init(0)
  val queue = Mem(gen,entries) /* also can use the Vec to instead of it*/

  when(io.enq.fire){
    waddr := waddr + 1
    len := len + 1
  }
  queue.write(waddr,io.enq.payload,enable = io.enq.fire) /* write the queue */

  when(io.deq.pop > 0){
    raddr := raddr + io.deq.pop
    len := len - io.deq.pop + io.enq.fire.asUInt
  }

  io.enq.ready := len < entries
  io.len := len
  (0 until heads).foreach {
    head =>
      io.deq.valids(head) := len > head
      io.deq.payloads(head) := queue.readAsync(raddr + head)
  }
  assert(io.deq.pop <= len && io.deq.pop.resize(log2Up(heads) + 1) <= heads && io.deq.pop.resize(log2Up(maxpop) + 1) <= maxpop) // assertion require resize it
}


object MultiHeadedQueue{
  def apply[T <: Data](src:Stream[T],entries:Int,heads:Int,maxpop:Int = 2) = {
    val MHqueue = new MultiHeadedQueue(cloneOf(src.payloadType),entries,heads,maxpop = maxpop)
    MHqueue.io.enq.connectFrom(src)
    (MHqueue.io.enq,MHqueue.io.len)
  }
}