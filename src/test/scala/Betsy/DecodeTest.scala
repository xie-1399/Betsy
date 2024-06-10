package Betsy

/* the decode test contain all kinds of data single test one by one */
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import BetsyLibs._
import spinal.lib.sim._
import scala.util.Random
class DecodeTest extends AnyFunSuite{
  /* Todo with assertion */
  test("decode"){
    SIMCFG().compile{
      val dut = new Top(SInt(4 bits),Architecture.tiny(),debug = true)
      dut.systolicArray.io.weight.simPublic()
      dut.systolicArray.array.mac.foreach(_.foreach(_.weight.simPublic()))
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(1 us)
        dut.clockDomain.forkStimulus(10)

        def init() = {
          dut.io.instruction.valid #= false
          dut.io.instruction.payload.randomize()
          dut.clockDomain.waitSampling()
        }

        def loadWeight(testCase: Int) = {
          var idx = 0
          val zeroes = Array.fill(testCase){Random.nextInt(10) > 5}
          val strides = Array.fill(testCase){Random.nextInt(8)}
          val addressList = Array.fill(testCase){Random.nextInt(256)}
          dut.clockDomain.onSamplings{
            if(dut.systolicArray.io.weight.valid.toBoolean && dut.systolicArray.io.weight.ready.toBoolean){
              println(s"load weight data : ${dut.systolicArray.io.weight.payload.toArray.map(_.toBigInt).mkString(",")}")
            }
          }
          while (idx < testCase){
            val zero = zeroes(idx)
            val size = 8
            val stride = strides(idx)
            val address = addressList(idx)
            dut.io.instruction.valid #= true
            val payload = InstructionGen.loadWeightGen(zero, size, stride, address, Architecture.tiny())
            dut.io.instruction.payload #= payload
            dut.clockDomain.waitSampling()
            if(dut.io.instruction.ready.toBoolean){
              val macweights = dut.systolicArray.array.mac.map(_.map(_.weight.toBigInt).toArray).toArray
              macweights.foreach {
                weight =>
                  println(weight.mkString(","))
              }
              if (dut.io.instruction.valid.toBoolean && !zero) {
                println(s"from ${address} load ${size} data into systolic array with stride ${stride} ...")
              } else if (dut.io.instruction.valid.toBoolean && zero) {
                println(s"load ${size} size 0 into the systolic array ... ")
              }
              idx += 1
              init()
            }
          }
        }
        init()
        loadWeight(4)
        simSuccess()
    }

  }
}
