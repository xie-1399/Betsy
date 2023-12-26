package Utils

import spinal.core._
import spinal.lib._

class FIFO[T<:Data](gen:HardType[T],val entries:Int,withFlush:Boolean = false,withNum:Boolean = false) extends Component{

  val io = new Bundle{
    val enqueue = slave (Stream(gen))
    val dequeue = master (Stream(gen))
    val flush = withFlush.generate(in Bool())
    val flushValue = withFlush.generate(in (gen))
    val flushDone = withFlush.generate(out Bool())
    val count = withNum.generate(out UInt(log2Up(entries) bits)) /* show the number of element in the fifo */
  }

  val enq_ptr = Counter(entries).init(0)
  val deq_ptr = Counter(entries).init(0)
  val counter = CounterUpDown(
    stateCount = entries,
    incWhen = io.enqueue.fire,
    decWhen = io.dequeue.fire
  )

  val queue = Mem(gen,entries)
  if(withFlush){io.flushDone := RegNext(MemOperation.flush(io.flush,queue,io.flushValue))}
  val full_empty = RegInit(False)
  io.dequeue.payload := queue.readAsync(deq_ptr)

  when(io.enqueue.fire){
    /* into the queue */
    queue.write(enq_ptr,io.enqueue.payload)
    enq_ptr.increment()
  }

  when(io.dequeue.fire){
    /* out the queue */
    deq_ptr.increment()
  }

  val empty = enq_ptr === deq_ptr && !full_empty
  val full = enq_ptr === deq_ptr && full_empty

  /* the key concept about the difference enqueue and dequeue */
  when(io.enqueue.fire =/= io.dequeue.fire){
    full_empty := io.enqueue.fire
  }

  io.enqueue.ready := !full
  io.dequeue.valid := !empty
  if(withNum) io.count := counter.value

  if(withFlush) {when(io.flush) {io.enqueue.ready := False;io.dequeue.valid := False}}
}


object FIFO{
  def apply[T<: Data](gen:HardType[T],entries:Int) = {
    new FIFO(gen,entries)
  }

  def apply[T <: Data](gen: HardType[T], entries: Int,withFlush:Boolean) = {
    new FIFO(gen,entries,withFlush)
  }
}
