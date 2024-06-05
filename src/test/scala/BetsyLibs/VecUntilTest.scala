package BetsyLibs

import Betsy.SimTools
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.util.Random

/* check the vec add and clip value is right
 * signal is PASS */

class VecUntilTest extends AnyFunSuite{

  test("stream vec add"){
    SIMCFG().compile{
      val dut = new VecAdder(UInt(4 bits),4)
      dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(1 us)
        dut.clockDomain.forkStimulus(10)

        def testCase = 1024
        var leftcheck = 0
        var rightcheck = 0
        var check = 0
        val left = Array.fill(testCase){Array.fill(4){Random.nextInt(16)}}
        val right = Array.fill(testCase){Array.fill(4){Random.nextInt(16)}}

        dut.io.left.valid #= false
        dut.io.right.valid #= false
        dut.io.output.ready #= false
        dut.clockDomain.waitSampling()
        StreamReadyRandomizer(dut.io.output,dut.clockDomain)
        StreamDriver(dut.io.left,dut.clockDomain){
          payload =>
            payload.zipWithIndex.foreach(p => p._1 #= left(leftcheck)(p._2))
            leftcheck += 1
            true
        }

        StreamDriver(dut.io.right, dut.clockDomain) {
          payload =>
            payload.zipWithIndex.foreach(p => p._1 #= right(rightcheck)(p._2))
            rightcheck += 1
            true
        }

        StreamMonitor(dut.io.output,dut.clockDomain){
          payload =>
            val ref = SimTools.vectorAdd(left(check),right(check)).map(r => SimTools.clipValue(4,false,r))
            val res = payload.map(_.toInt).toArray
            assert(ref.sameElements(res))
            check += 1
        }
        dut.clockDomain.waitSamplingWhere(check == testCase - 1)
        simSuccess()
    }
  }

}
