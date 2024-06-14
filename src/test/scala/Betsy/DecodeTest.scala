package Betsy

/* the decode test contain all kinds of data single test one by one */
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import BetsyLibs._
import spinal.lib.sim._
import spinal.lib._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import SimTools._

class DecodeTest extends AnyFunSuite{

  def init(dut:Top[SInt]) = {
    dut.clockDomain.forkStimulus(10)
    dut.io.instruction.valid #= false
    dut.io.instruction.payload.randomize()
    dut.clockDomain.waitSampling()
  }

  val memoryContent = genMemoryValue(4,8,8192) /* for the tiny memory content */

  test("Load_Weight"){
    SIMCFG().compile{
      val arch = Architecture.tiny()
      val dut = new Top(SInt(4 bits),arch,initContent = (0 until arch.localDepth.toInt).toArray.map(_.toBigInt))
      dut.systolicArray.io.weight.simPublic()
      dut.systolicArray.array.mac.foreach(_.foreach(_.weight.simPublic()))
      dut.systolicArray.array.bias.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 us)
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
              dut.io.instruction.payload #= InstructionGen.noOpGen(Architecture.tiny())
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
              assert(dut.systolicArray.array.bias.map(_.toBigInt == 0).reduce(_&&_), "the bias is not zero")
            }
          }
        }
        init(dut)
        loadWeight(1024)
        simSuccess()
    }
  }

  test("matmul"){
    SIMCFG().compile {
      val arch = Architecture.tiny()
      val dut = new Top(SInt(4 bits), arch, initContent = memoryContent)
      dut.systolicArray.io.weight.simPublic()
      dut.systolicArray.array.mac.foreach(_.foreach(_.weight.simPublic()))
      dut.systolicArray.array.bias.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 us)
        dut.clockDomain.forkStimulus(10)
        /* first load some weights in the 8 × 8 array */

        /* when loading the weight */
        def Load(address:Int,stride:Int,size:Int,loadMatrixPrint:Boolean = false) = {
          init(dut)
          println(s"load the memory content at address ${address} with ${size} size and ${stride} stride ")

          val payload = InstructionGen.loadWeightGen(false, size , stride, address, Architecture.tiny())
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= payload
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)

          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= InstructionGen.loadWeightGen(true, 1, 0, 0, Architecture.tiny()) //load zeroes
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)

          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= InstructionGen.noOpGen(Architecture.tiny())
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)

          if(loadMatrixPrint){
            val arrayWeight = dut.systolicArray.array.mac.map(_.map(_.weight.toBigInt).toArray).toArray
            arrayWeight.foreach{
              weight =>
                println(weight.mkString(","))
            }
          }
        }

        def LoadTest(testCase:Int) = {
          for(idx <- 0 until testCase){
            val address = Random.nextInt(1024)
            val stride = Random.nextInt(4)
            val step = 1 << stride
            val size = 8
            val contentArray = new ArrayBuffer[BigInt]()
            for (idx <- address until address + size * step by step) {
              contentArray += memoryContent(idx)
            }
            Load(address,stride,8,false)
            val refMatrix = MemoryContentToMatrix(contentArray.toArray,size,4)
            val refVec = refMatrix.flatten
            val loadMatrix = dut.systolicArray.array.mac.map(_.map(_.weight.toBigInt).toArray).toArray
            val loadVec = loadMatrix.flatten
            assert(refVec.sameElements(loadVec),"load value error!!!")
            contentArray.clear()
          }
        }

        init(dut)
        LoadTest(128)
        simSuccess()
    }
  }


  test("configure"){
    /* configure the regs in the top module */
    SIMCFG().compile{
      SimTimeout(10 us)
      val arch = Architecture.tiny()
      val dut = new Top(SInt(4 bits), arch)
      dut.decode.io.pc.simPublic()
      dut.decode.runCycles.simPublic()
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)
        init(dut)

        def update(register:Int,value:Int) = {
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= InstructionGen.configureGen(register,value,Architecture.tiny())
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          dut.io.instruction.valid #= false
          dut.clockDomain.waitSampling()
        }

        val configure = fork{
          val randomNoop = Random.nextInt(1024)
          println(s"running NoOp instruction numbers : ${randomNoop}")
          var pc = 0
          while(pc != randomNoop * 8){
            dut.io.instruction.valid.randomize()
            dut.io.instruction.payload #= InstructionGen.noOpGen(Architecture.tiny())
            dut.clockDomain.waitSampling()
            if(dut.io.instruction.valid.toBoolean && dut.io.instruction.ready.toBoolean){
              pc = pc + 8
            }
            dut.io.instruction.valid #= false
            dut.clockDomain.waitSampling()
            assert(dut.decode.io.pc.toBigInt == pc , "pc value error !!!")
            assert(dut.decode.runCycles.toBigInt == pc / 8, "run cycle value error")
          }
          println(s"current pc : ${dut.decode.io.pc.toBigInt}")
          println(s"running cycles : ${dut.decode.runCycles.toBigInt}")

          update(10,2048) /* update pc */
          update(9,1024)  /* update run cycles */
          println(s"current pc : ${dut.decode.io.pc.toBigInt}")
          println(s"running cycles : ${dut.decode.runCycles.toBigInt}")

          simSuccess()
        }
    }

  }
}
