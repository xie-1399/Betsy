package BetsyUntils

import spinal.core._
import scala.util.Random
import BetsyUntils.SIMCFG

object MultiHeadedQueueTest extends App{
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new MultiHeadedQueue(UInt(3 bits),64,heads = 4,maxpop = 4)
    dut
  }.doSimUntilVoid{
    dut =>
      def testCase = 1024
      dut.clockDomain.forkStimulus(10)
      val payloads = Array.fill(testCase)(Random.nextInt(8))
      dut.io.enq.valid #= false
      dut.io.deq.pop #= 0
      dut.clockDomain.waitSampling()
      var pushes = 0
      var popes = 0
      /* fill the mem */
      val pushThread = fork{
        while (true){
          dut.io.enq.valid.randomize()
          if(pushes == testCase){dut.io.enq.valid #= false}
          if(pushes == testCase){dut.io.enq.payload #= 0} else{dut.io.enq.payload #= payloads(pushes)}
          dut.clockDomain.waitSampling()
          if(dut.io.enq.valid.toBoolean && dut.io.enq.ready.toBoolean){
            pushes += 1
          }
        }
      }

      val popThread = fork{
        while(true){
          dut.io.deq.pop #= 0
          val randomPop = Random.nextInt(5)
          if(pushes >= randomPop + popes && randomPop != 0){
            dut.io.deq.pop #= randomPop
            dut.clockDomain.waitSampling()
            for(idx <- 0 until randomPop) {
              assert(dut.io.deq.payloads(idx).toInt == payloads(popes + idx))
            }
            popes += randomPop
            if(popes == testCase){simSuccess()}
          }else{
            dut.clockDomain.waitSampling()
          }
        }
      }
  }

}
