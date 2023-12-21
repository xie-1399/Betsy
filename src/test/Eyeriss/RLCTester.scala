package Eyeriss

import Eyeriss.RLC._
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
      val dut = new Encode(config)
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)

        dut.io.dataIn.valid #= false
        dut.clockDomain.waitSampling()

        val refQueue = mutable.Queue[Int]()

        def setZeroNot(zeroNum:Int,num:Int = 0) = {
            for(idx <- 0 until zeroNum){
              dut.io.dataIn.valid #= true
              dut.io.dataIn.payload #= num
              dut.io.dataOut.ready #= true
              dut.clockDomain.waitSamplingWhere(dut.io.dataIn.ready.toBoolean)
            }
        }

        def checkIt(binStr:String) = {
          val dutArray = ArrayBuffer[BigInt]()
          require(binStr.length == 64)
          var level = true
          var temp = ""
          for(idx <- 1 until binStr.length){
            if(level){
              temp += binStr(idx)
              if(temp.length == 16){
                dutArray += BigInt(temp,2)
                temp = ""
                level = false
              }
            }else{
              temp += binStr(idx)
              if (temp.length == 5) {
                dutArray += BigInt(temp, 2)
                temp = ""
                level = true
              }
            }
          }
          dutArray
        }

        for(iter <- 0 until 1024 * 16){
          for (idx <- 0 until 3) {
            val randZero = Random.nextInt(31) + 1
            val randPayload = Random.nextInt(4096) + 1
            refQueue.enqueue(randZero); refQueue.enqueue(randPayload)
            setZeroNot(randZero)
            setZeroNot(zeroNum = 1, num = randPayload)
          }
          dut.clockDomain.waitSamplingWhere(dut.io.compress.toBoolean)
          if (dut.io.dataOut.valid.toBoolean) {
            StringSplitShow(StringWithWidth(dut.io.dataOut.payload.toBigInt.toString(2), 64), 21, down = true)
          }
          val dutarray = checkIt(StringWithWidth(dut.io.dataOut.payload.toBigInt.toString(2), 64))

          for (idx <- dutarray.length - 1 to 0 by -1) {
            assert(dutarray(idx) == refQueue.dequeue())
          }
        }

        simSuccess()
    }
  }

}
