package Betsy

import spinal.core._

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
          val randomPush = Random.nextInt(5)
          if(randomPush != 0 && randomPush + pushes <= testCase && randomPush < avail){
            dut.io.enq.push #= randomPush
            for(idx <- 0 until randomPush) {dut.io.enq.payloads(idx) #= payloads(idx + pushes)}
            dut.clockDomain.waitSampling()
            pushes = pushes + randomPush
            avail = avail - randomPush
          }else{
            dut.io.enq.push #= 0
            dut.clockDomain.waitSampling()
          }
        }
      }
      val popThread = fork { /* check the pop data */
        while (true) {
          dut.io.deq.ready.randomize()
          dut.clockDomain.waitSampling()
          if(dut.io.deq.valid.toBoolean && dut.io.deq.ready.toBoolean){
            assert(dut.io.deq.payload.toBigInt == payloads(popes))
            popes += 1
            avail += 1
            if(popes == testCase){simSuccess()}
          }
        }
      }
      pushThread.join()
      popThread.join()
      simSuccess()
  }

}
