package Eyeriss

import Eyeriss.RLC.Decode
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import Untils.SIMCFG
import Untils.TesterUntils._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// Todo assert the decode and fix the bug

class RLCTester extends AnyFunSuite{

  val config = EyerissParameters()

  def GenRunLevels(num:Int) = {
    val runsArray = ArrayBuffer[String]()
    val levelsArray = ArrayBuffer[String]()
    val ref = ArrayBuffer[Int]()
    (0 until num).foreach{
      i =>
        val runRandom = Random.nextInt(32)
        val levelRandom = Random.nextInt(4096)
        ref +=(runRandom);ref.+= (levelRandom)
        runsArray += StringWithWidth(runRandom.toBinaryString,5)
        levelsArray += StringWithWidth(levelRandom.toBinaryString,16)
    }
    (runsArray,levelsArray,ref)
  }

  test("decode"){
    SIMCFG().compile{
      val dut = new Decode(config)
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)

        def init() = {
          dut.io.dataIn.valid #= false
          dut.clockDomain.waitSampling()
        }

        def CatchAndAssert() = {
          dut.clockDomain.onSamplings {
            val fire = dut.io.dataOut.valid.toBoolean && dut.io.dataOut.ready.toBoolean
            if (fire) {
              println(dut.io.dataIn.payload.toBigInt)
            }
          }
        }

        def SetCode(showIt:Boolean = false) = {
          val gen = GenRunLevels(3)
          val runsArray = gen._1
          val levelsArray = gen._2
          val ref = gen._3
          var refString = "0"
          (0 until 3).foreach{
            idx =>
              refString += runsArray(idx)
              refString += levelsArray(idx)
          }
          val payload = BigInt(refString,2)
          if (showIt) {
            println("ref String ")
            StringSplitShow(refString, 16)
            println("ref value ")
            print(ref.mkString(",") + "\n")
          }
          dut.io.dataIn.valid #= true
          dut.io.dataIn.payload #= payload
          dut.io.dataOut.ready #= true
          dut.clockDomain.waitSamplingWhere(dut.io.dataIn.ready.toBoolean)
          dut.io.dataIn.valid #= false
          dut.clockDomain.waitSamplingWhere(dut.io.end.toBoolean)
        }


        init()
        CatchAndAssert()
        SetCode(true)

        simSuccess()
    }

  }


  test("encode"){

  }

}
