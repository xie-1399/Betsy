package BetsyLibs

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import org.scalatest.funsuite.AnyFunSuite

class BetsyFIFOTest extends AnyFunSuite {
  test("FIFO random test") {
    SIMCFG().compile {
      val dut = new BetsyFIFO(Bits(10 bits),32)
      dut.empty.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        val queueModel = mutable.Queue[Long]()
        SimTimeout(1000000 * 10)
        def monitor() = {
          /* Push data randomly, and fill the queueModel with pushed transactions. */
          val pushThread = fork {
            dut.io.push.valid #= false
            while (true) {
              dut.io.push.valid.randomize()
              dut.io.push.payload.randomize()
              dut.clockDomain.waitSampling()
              if (dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean) {
                queueModel.enqueue(dut.io.push.payload.toLong)
              }
            }
          }
          /* Pop data randomly, and check that it match with the queueModel. */
          val popThread = fork {
            dut.io.pop.ready #= true
            for (i <- 0 until 1000) {
              dut.io.pop.ready.randomize()
              dut.clockDomain.waitSampling()
              if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) {
                assert(dut.io.pop.payload.toLong == queueModel.dequeue())
              }
            }
            simSuccess()
          }
        }
        monitor()
    }
  }

}
