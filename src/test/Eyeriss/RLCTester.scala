package Eyeriss

import Eyeriss.RLC.Decode
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import Untils.SIMCFG
import Untils.TesterUntils._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class RLCTester extends AnyFunSuite{

  val config = EyerissParameters()
  val refQueue = mutable.Queue[Int]()
  def GenRunLevels(num:Int) = {
    val runsArray = ArrayBuffer[String]()
    val levelsArray = ArrayBuffer[String]()
    val ref = ArrayBuffer[Int]()
    (0 until num).foreach{
      i =>
        val runRandom = Random.nextInt(31) + 1
        val levelRandom = Random.nextInt(4096) + 1
        ref +=(runRandom);ref+= (levelRandom);
        refQueue.enqueue(runRandom)
        refQueue.enqueue(levelRandom)
        runsArray += StringWithWidth(runRandom.toBinaryString,5)
        levelsArray += StringWithWidth(levelRandom.toBinaryString,16)
    }
    (runsArray,levelsArray,ref)
  }

  def GenPayload(showIt:Boolean = false) = {
    val gen = GenRunLevels(3)
    val runsArray = gen._1
    val levelsArray = gen._2
    val ref = gen._3
    var refString = "0"
    (0 until 3).foreach {
      idx =>
        refString += levelsArray(2 - idx)
        refString += runsArray(2 - idx)
    }
    val payload = BigInt(refString, 2)
    if (showIt) {
      println("ref String ")
      StringSplitShow(refString, 21, down = true)
    }
    payload
  }

  test("decode"){
    SIMCFG().compile{
      val dut = new Decode(config)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        var zeroNum = 0
        dut.clockDomain.onSamplings {
          val fire = dut.io.dataOut.valid.toBoolean && dut.io.dataOut.ready.toBoolean
          if (fire) {
            if (dut.io.dataOut.payload.toBigInt == 0) {
              zeroNum += 1
            } else {
              assert(zeroNum == refQueue.dequeue())
              assert(dut.io.dataOut.payload.toBigInt == refQueue.dequeue())
              zeroNum = 0
            }
          }
        }
        for(idx <- 0 until 10000){
          refQueue.clear()
          dut.io.dataIn.valid #= false
          dut.clockDomain.waitSampling()
          dut.io.dataIn.valid #= true
          dut.io.dataIn.payload #= GenPayload()
          dut.io.dataOut.ready #= true
          dut.clockDomain.waitSamplingWhere(dut.io.dataIn.ready.toBoolean)
          dut.io.dataIn.valid #= false
          dut.clockDomain.waitSamplingWhere(dut.io.end.toBoolean)
        }
        simSuccess()
    }

  }


  test("encode"){
    SIMCFG().compile {
      val dut = new Decode(config)
      dut
    }.doSimUntilVoid {
      dut =>


    }
  }

}
