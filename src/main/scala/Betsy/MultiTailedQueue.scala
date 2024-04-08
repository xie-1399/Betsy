package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

import spinal.core._
import spinal.lib._
import Until._

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

  // assert(io.enq.push < io.enq.up && io.enq.up <= avail)
}

object MultiTailedQueue{
  def apply[T <:Data](gen:HardType[T],entries:Int,maxpush:Int) = {
    new MultiTailedQueue(gen,entries,maxpush)
  }
}


object MultiTailedQueueTest extends App{
  import spinal.core.sim._
  import scala.util.Random
  SIMCFG().compile{
    val dut = new MultiTailedQueue(UInt(3 bits),64,maxpush = 4)
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      def testCase = 1024
      val payloads = Array.fill(testCase)(Random.nextInt(8))
      dut.io.enq.push #= 0
      dut.clockDomain.waitSampling()
      var pushes = 0
      var popes = 0
      var avail = 64
      /* push into the queue */
      val pushThread = fork {
        while (true) {
          dut.io.enq.push #= 0
          val randomPush = Random.nextInt(4)
          if(randomPush < avail && randomPush != 0){
            dut.io.enq.push #= randomPush
            for(idx <- 0 to randomPush) {dut.io.enq.payloads(idx) #= payloads(idx + pushes)}
            dut.clockDomain.waitSampling()
            pushes = pushes + randomPush
            avail = avail - randomPush
          }else{
            dut.clockDomain.waitSampling()
          }
          if(pushes == testCase) {simSuccess()}
        }
      }
      val popThread = fork { /* check the pop data */
        while (true) {
          dut.io.deq.ready.randomize()
          dut.clockDomain.waitSampling()
          if(dut.io.deq.valid.toBoolean && dut.io.deq.ready.toBoolean){
            assert(dut.io.deq.payload.toBigInt == payloads(popes))
            popes += 1
            if(popes == testCase){simSuccess()}
          }
        }
      }
      pushThread.join()
      popThread.join()
      simSuccess()
  }

}