package Betsy

/* the decode test contain all kinds of data single test one by one */
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import BetsyLibs._
import spinal.lib.sim._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import SimTools._

class DecodeTest extends AnyFunSuite{

  test("Load_Weight"){
    SIMCFG().compile{
      val dut = new Top(SInt(4 bits),Architecture.tiny(),debug = true)
      dut.systolicArray.io.weight.simPublic()
      dut.systolicArray.array.mac.foreach(_.foreach(_.weight.simPublic()))
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 us)
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
          val addressList = Array.fill(testCase){Random.nextInt(1024)}
          while (idx < testCase){
            val zero = zeroes(idx)
            val size = 8
            val stride = strides(idx)
            val address = addressList(idx)
            dut.io.instruction.valid #= true
            val payload = InstructionGen.loadWeightGen(zero, size, stride, address, Architecture.tiny())
            dut.io.instruction.payload #= payload
            dut.clockDomain.waitSampling()
            if(dut.io.instruction.ready.toBoolean) {
              if (dut.io.instruction.valid.toBoolean && !zero) {
                println(s"from ${address} load ${size} data into systolic array with stride ${stride} ...")
              } else if (dut.io.instruction.valid.toBoolean && zero) {
                println(s"load ${size} size 0 into the systolic array ... ")
              }
              dut.io.instruction.valid #= true
              dut.io.instruction.payload #= InstructionGen.loadWeightGen(true, 1, 0, 0, Architecture.tiny()) //load zeroes
              dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)

              dut.io.instruction.valid #= true
              dut.io.instruction.payload #= InstructionGen.NoopGen(Architecture.tiny())
              dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)

              // init()
              idx += 1
              if (zero) {
                val ref = Array.fill(Architecture.tiny().arraySize) {
                  Array.fill(8) {
                    0
                  }
                }.flatten
                val test = dut.systolicArray.array.mac.map(_.map(_.weight.toBigInt).toArray).toArray.flatten
                assert(ref.sameElements(test), "load the all zeroes failed!!!")
              } else {
                val test = dut.systolicArray.array.mac.map(_.map(_.weight.toBigInt).toArray).toArray
                val testArray = loadMatrixReorder(test).reverse
                val step = 1 << stride
                val ref = Range(address,address + size * step,step).toArray
                assert(ref.sameElements(testArray),"load the value error!!!")
              }
            }
          }
        }

        init()
        loadWeight(1024)
        simSuccess()
    }

  }
}
