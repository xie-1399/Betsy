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
        dut.clockDomain.forkStimulus(10)
        println("start testing the Load Instruction ================>  ")
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
        println("load instruction test success!")
        simSuccess()
    }
  }

  // Todo tested it
  test("matmul"){
    SIMCFG().compile {
      val arch = Architecture.tiny()
      val dut = new Top(SInt(4 bits), arch, initContent = memoryContent)
      dut.systolicArray.io.weight.simPublic()
      dut.systolicArray.array.mac.foreach(_.foreach(_.weight.simPublic()))
      dut.systolicArray.array.bias.simPublic()
      dut.accumulatorWithALUArray.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(1 us)
        dut.clockDomain.forkStimulus(10)
        /* first load some weights in the 8 × 8 array */
        println("start testing the MatMul Instruction ================>  ")
        /* when loading the weight */
        def Load(address:Int,stride:Int,size:Int,loadMatrixPrint:Boolean = false) = {
          init(dut)
            println(s"========== load the memory content at address ${address} with ${size} size and ${stride} stride ======= ")

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

        def Matmul(zero: Boolean, localAddress: Int, localStride: Int,
                   accumulatorAddress: Int, accumulatorStride: Int, size: Int) = {
          init(dut)
          // load weight is ready
          val payload = InstructionGen.matMulGen(Architecture.tiny(), zero, localAddress, localStride, accumulatorAddress, accumulatorStride, size)._1
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= payload
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)

          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= InstructionGen.noOpGen(Architecture.tiny())
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
        }

        def MatmulTest(testCase: Int) = {
          for (test <- 0 until testCase) {
            val accwriteMatrix = new ArrayBuffer[Array[Int]]()
            dut.clockDomain.onSamplings {
              if (dut.accumulatorWithALUArray.io.control.valid.toBoolean && dut.accumulatorWithALUArray.io.control.ready.toBoolean
                && dut.accumulatorWithALUArray.io.control.write.toBoolean) {
                accwriteMatrix += dut.accumulatorWithALUArray.io.inputs.payload.map(_.toInt).toArray
              }
            }
            val address = Random.nextInt(1024)
            val stride = Random.nextInt(4)
            val step = 1 << stride
            val loadsize = 8
            val contentArray = new ArrayBuffer[BigInt]()
            for (idx <- address until address + loadsize * step by step) {
              contentArray += memoryContent(idx)
            }
            Load(address, stride, 8, false)
            val refMatrix = MemoryContentToMatrix(contentArray.toArray, loadsize, 4)
            val refVec = refMatrix.flatten
            val loadMatrix = dut.systolicArray.array.mac.map(_.map(_.weight.toBigInt).toArray).toArray
            val loadVec = loadMatrix.flatten
            assert(refVec.sameElements(loadVec), "load value error!!!")
            contentArray.clear()

            val zero = false
            val localAddress = Random.nextInt(1024)
            val localStride = Random.nextInt(4)
            val accumulatorAddress = Random.nextInt(1024)
            val accumulatorStride = 0 //Todo
            val inputsize = 8
            val inputstep = 1 << localStride
            for (idx <- localAddress until localAddress + inputsize * inputstep by inputstep) {
              contentArray += memoryContent(idx)
            }
            val inputMatrix = MemoryContentToMatrix(contentArray.toArray, inputsize, 4)
            Matmul(zero,localAddress, localStride, accumulatorAddress, accumulatorStride, inputsize)
            val refGemm = Matrix.multiply(inputMatrix.map(_.map(_.toInt)),refMatrix.map(_.map(_.toInt)))
            println("input matrix:")
            inputMatrix.foreach{
              input =>
                println(input.mkString(","))
            }

            println("weight matrix:")
            refMatrix.foreach {
              weight =>
                println(weight.mkString(","))
            }

            println("ref matrix:")
            refGemm.foreach{
              ref =>
                println(ref.mkString(","))
            }
            println("test matmul matrix : ")
            accwriteMatrix.foreach {
              test =>
                println(test.mkString(","))
            }
            contentArray.clear()
            accwriteMatrix.clear()
          }
          println("GEMM test success!  :)")
        }

        init(dut)
        MatmulTest(2)
        simSuccess()
    }
  }

  //Todo update
  test("configure"){
    /* configure the regs in the top module */
    SIMCFG().compile{
      val arch = Architecture.tiny()
      val dut = new Top(SInt(4 bits), arch)
      dut.decode.io.pc.simPublic()
      dut.decode.runCycles.simPublic()
      dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(10 us)
        println("start testing the Configure Instruction ================>  ")
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
