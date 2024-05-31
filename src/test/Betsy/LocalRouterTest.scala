package Betsy

import BetsyLibs.SIMCFG
import Betsy.LocalRouter
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.StreamReadyRandomizer
import SimTools._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

/* the local host should test all local move cases */
/**
 ** U(1) -> memory to weight
 ** U(2) -> memory to input
 ** U(3) -> array to acc
 ** U(4) -> memory to acc
 ** U(5) -> acc to memory
 **/

//Todo shrink the test
class LocalRouterTest extends AnyFunSuite{

  test("data move"){
    SIMCFG().compile{
      val arch = Architecture()
      val dut = new LocalRouter(Bits(16 bits),arch)
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)
        StreamReadyRandomizer(dut.io.memoryDataFlow.memIn,dut.clockDomain)
        StreamReadyRandomizer(dut.io.arrayDataFlow.weight,dut.clockDomain)
        StreamReadyRandomizer(dut.io.arrayDataFlow.input,dut.clockDomain)
        StreamReadyRandomizer(dut.io.accumulatorDataFlow.accIn,dut.clockDomain)

        def init() = {
          StreamInit(dut.io.control)
          StreamInit(dut.io.accumulatorDataFlow.accOut)
          StreamInit(dut.io.arrayDataFlow.output)
          StreamInit(dut.io.memoryDataFlow.memOut)
        }

        def memory2weight(iter:Int) = {
          println("memory to array weight test starts ...")
          var idx = 0
          val sizes = Array.fill(iter){Random.nextInt(1024)}
          while(idx < iter){
            dut.io.control.valid #= true
            dut.io.control.sel #= 1
            dut.io.control.size #= sizes(idx)
            val refBuffer = new ArrayBuffer[BigInt]()
            val testBuffer = new ArrayBuffer[BigInt]()
            refBuffer.clear()
            testBuffer.clear()
            println(s"trans size:${sizes(idx)}")
            while(!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)){
              val payload = Random.nextInt(1024)
              dut.io.memoryDataFlow.memOut.valid.randomize()
              dut.io.memoryDataFlow.memOut.payload #= payload
              dut.clockDomain.waitSampling()
              if(dut.io.memoryDataFlow.memOut.valid.toBoolean && dut.io.memoryDataFlow.memOut.ready.toBoolean
                && (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean))){
                refBuffer += dut.io.memoryDataFlow.memOut.payload.toBigInt
              }
              if(dut.io.arrayDataFlow.weight.valid.toBoolean && dut.io.arrayDataFlow.weight.ready.toBoolean
                && !(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)){
                testBuffer += dut.io.arrayDataFlow.weight.payload.toBigInt
              }
            }
            init()
            dut.clockDomain.waitSampling()

            assert(refBuffer.length == refBuffer.length && refBuffer.length == sizes(idx),"data length error !!!")
            assert(refBuffer.sameElements(testBuffer),"memory to weight error !!!")
            idx += 1
          }
          println("memory to array weight test ends ...\n")
        }

        def memory2Input(iter: Int) = {
          println("memory to array input test starts ...")
          var idx = 0
          val sizes = Array.fill(iter) {
            Random.nextInt(1024)
          }
          while (idx < iter) {
            dut.io.control.valid #= true
            dut.io.control.sel #= 2
            dut.io.control.size #= sizes(idx)
            val refBuffer = new ArrayBuffer[BigInt]()
            val testBuffer = new ArrayBuffer[BigInt]()
            refBuffer.clear()
            testBuffer.clear()
            println(s"trans size:${sizes(idx)}")
            while (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)) {
              val payload = Random.nextInt(1024)
              dut.io.memoryDataFlow.memOut.valid.randomize()
              dut.io.memoryDataFlow.memOut.payload #= payload
              dut.clockDomain.waitSampling()
              if (dut.io.memoryDataFlow.memOut.valid.toBoolean && dut.io.memoryDataFlow.memOut.ready.toBoolean
                && (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean))) {
                refBuffer += dut.io.memoryDataFlow.memOut.payload.toBigInt
              }
              if (dut.io.arrayDataFlow.input.valid.toBoolean && dut.io.arrayDataFlow.input.ready.toBoolean
                && !(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)) {
                testBuffer += dut.io.arrayDataFlow.input.payload.toBigInt
              }
            }
            init()
            dut.clockDomain.waitSampling()

            assert(refBuffer.length == refBuffer.length && refBuffer.length == sizes(idx), "data length error !!!")
            assert(refBuffer.sameElements(testBuffer), "memory to input error !!!")
            idx += 1
          }
          println("memory to array input test ends ...\n")
        }

        def acc2memory(iter: Int) = {
          println("acc to memory test starts ...")
          var idx = 0
          val sizes = Array.fill(iter) {
            Random.nextInt(1024)
          }
          while (idx < iter) {
            dut.io.control.valid #= true
            dut.io.control.sel #= 5
            dut.io.control.size #= sizes(idx)
            val refBuffer = new ArrayBuffer[BigInt]()
            val testBuffer = new ArrayBuffer[BigInt]()
            refBuffer.clear()
            testBuffer.clear()
            println(s"trans size:${sizes(idx)}")
            while (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)) {
              val payload = Random.nextInt(1024)
              dut.io.accumulatorDataFlow.accOut.valid.randomize()
              dut.io.accumulatorDataFlow.accOut.payload #= payload
              dut.clockDomain.waitSampling()
              if (dut.io.accumulatorDataFlow.accOut.valid.toBoolean && dut.io.accumulatorDataFlow.accOut.ready.toBoolean
                && (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean))) {
                refBuffer += dut.io.accumulatorDataFlow.accOut.payload.toBigInt
              }
              if (dut.io.memoryDataFlow.memIn.valid.toBoolean && dut.io.memoryDataFlow.memIn.ready.toBoolean
                && !(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)) {
                testBuffer += dut.io.memoryDataFlow.memIn.payload.toBigInt
              }
            }
            init()
            dut.clockDomain.waitSampling()

            assert(refBuffer.length == refBuffer.length && refBuffer.length == sizes(idx), "data length error !!!")
            assert(refBuffer.sameElements(testBuffer), "acc to memory error !!!")
            idx += 1
          }
          println("acc to memory test ends ...\n")
        }

        def array2acc(iter: Int) = {
          println("array to acc test starts ...")
          var idx = 0
          val sizes = Array.fill(iter) {
            Random.nextInt(1024)
          }
          while (idx < iter) {
            dut.io.control.valid #= true
            dut.io.control.sel #= 3
            dut.io.control.size #= sizes(idx)
            val refBuffer = new ArrayBuffer[BigInt]()
            val testBuffer = new ArrayBuffer[BigInt]()
            refBuffer.clear()
            testBuffer.clear()
            println(s"trans size:${sizes(idx)}")
            while (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)) {
              val payload = Random.nextInt(1024)
              dut.io.arrayDataFlow.output.valid.randomize()
              dut.io.arrayDataFlow.output.payload #= payload
              dut.clockDomain.waitSampling()
              if (dut.io.arrayDataFlow.output.valid.toBoolean && dut.io.arrayDataFlow.output.ready.toBoolean
                && (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean))) {
                refBuffer += dut.io.arrayDataFlow.output.payload.toBigInt
              }
              if (dut.io.accumulatorDataFlow.accIn.valid.toBoolean && dut.io.accumulatorDataFlow.accIn.ready.toBoolean
                && !(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)) {
                testBuffer += dut.io.accumulatorDataFlow.accIn.payload.toBigInt
              }
            }
            init()
            dut.clockDomain.waitSampling()

            assert(refBuffer.length == refBuffer.length && refBuffer.length == sizes(idx), "data length error !!!")
            assert(refBuffer.sameElements(testBuffer), "array to acc error !!!")
            idx += 1
          }
          println("array to acc test ends ...\n")
        }

        def memory2acc(iter: Int) = { //longest
          println("memory to acc test starts ...")
          var idx = 0
          val sizes = Array.fill(iter) {
            Random.nextInt(1024)
          }
          while (idx < iter) {
            dut.io.control.valid #= true
            dut.io.control.sel #= 4
            dut.io.control.size #= sizes(idx)
            val refBuffer = new ArrayBuffer[BigInt]()
            val testBuffer = new ArrayBuffer[BigInt]()
            refBuffer.clear()
            testBuffer.clear()
            println(s"trans size:${sizes(idx)}")
            while (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)) {
              val payload = Random.nextInt(1024)
              dut.io.memoryDataFlow.memOut.valid.randomize()
              dut.io.memoryDataFlow.memOut.payload #= payload
              dut.clockDomain.waitSampling()
              if (dut.io.memoryDataFlow.memOut.valid.toBoolean && dut.io.memoryDataFlow.memOut.ready.toBoolean
                && (!(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean))) {
                refBuffer += dut.io.memoryDataFlow.memOut.payload.toBigInt
              }
              if (dut.io.accumulatorDataFlow.accIn.valid.toBoolean && dut.io.accumulatorDataFlow.accIn.ready.toBoolean
                && !(dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)) {
                testBuffer += dut.io.accumulatorDataFlow.accIn.payload.toBigInt
              }
            }
            init()
            dut.clockDomain.waitSampling()

            assert(refBuffer.length == refBuffer.length && refBuffer.length == sizes(idx), "data length error !!!")
            assert(refBuffer.sameElements(testBuffer), "memory to acc error !!!")
            idx += 1
          }
          println("memory to acc test ends ...\n")
        }

        def testCase = 128
        init()
        dut.clockDomain.waitSampling()

        memory2weight(testCase)
        memory2Input(testCase)
        acc2memory(testCase)
        array2acc(testCase)
        memory2acc(testCase)
        simSuccess()
    }
  }

}
