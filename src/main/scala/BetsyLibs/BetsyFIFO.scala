package BetsyLibs

/*
 * the spinal libs has Stream fifo
 * this file will make a more friendly use fifo
 * the fifo control is more simple to use
*/
import Betsy.Until.BetsyModule
import spinal.core._
import spinal.lib._

class BetsyFIFO[T <: Data](gen: HardType[T], val entries: Int) extends BetsyModule {

  val io = new Bundle {
    val push = slave(Stream(gen))
    val pop = master(Stream(gen))
  }

  val enq_ptr = Counter(entries).init(0)
  val deq_ptr = Counter(entries).init(0)

  val queue = Mem(gen, entries)
  val full_empty = RegInit(False)
  io.pop.payload := queue.readAsync(deq_ptr)

  when(io.push.fire) {
    /* into the queue */
    queue.write(enq_ptr, io.push.payload)
    enq_ptr.increment()
  }

  when(io.pop.fire) {
    /* out the queue */
    deq_ptr.increment()
  }

  val empty = enq_ptr === deq_ptr && !full_empty
  val full = enq_ptr === deq_ptr && full_empty

  /* the key concept about the difference push and pop */
  when(io.push.fire =/= io.pop.fire) {
    full_empty := io.push.fire
  }

  io.push.ready := !full
  io.pop.valid := !empty
}
