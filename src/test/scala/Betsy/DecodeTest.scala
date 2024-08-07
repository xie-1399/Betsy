package Betsy

/* the decode test contain all kinds of data single test one by one */
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import BetsyLibs._
import spinal.lib._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import SimTools._
import BetsyLibs.sim._

class DecodeTest extends AnyFunSuite{

  def init(dut: Top[SInt]): Unit = {
    AxiInit(dut.io.activationBus)
    AxiInit(dut.io.weightBus)
    dut.io.instruction.valid #= false
    dut.io.instruction.payload.randomize()
    dut.clockDomain.waitSampling()
  }

  val memoryContent: Array[BigInt] = genMemoryValue(8, 8, 2048) /* for the tiny memory content */

  test("load"){
    SIMCFG().compile{
      val arch = Architecture.tiny()
      val dut = new Top(SInt(8 bits),arch,initContent = (0 until arch.localDepth.toInt).toArray.map(_.toBigInt))
      dut.systolicArray.io.weight.simPublic()
      dut.systolicArray.array.mac.foreach(_.foreach(_.weight.simPublic()))
      dut.systolicArray.array.bias.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 us)
        dut.clockDomain.forkStimulus(10)
        println("start testing the Load Instruction ================>  ")
        def loadWeight(testCase: Int): Unit = {
          val zeroes = Array.fill(testCase) {Random.nextInt(10) > 5}
          val strides = Array.fill(testCase) {Random.nextInt(4)}
          val addressList = Array.fill(testCase) {Random.nextInt(1024)}
          def insertPipe(): Unit = {
            val payload = InstructionGen.loadWeightGen(true, 0, 0, 0, Architecture.tiny())
            dut.io.instruction.valid #= true
            dut.io.instruction.payload #= payload
            dut.clockDomain.waitSamplingWhere(dut.decode.instruction.ready.toBoolean)

            dut.io.instruction.valid #= true //nop or other instructions also work
            dut.io.instruction.payload #= InstructionGen.noOpGen(Architecture.tiny())
            dut.clockDomain.waitSamplingWhere(dut.decode.instruction.ready.toBoolean)
          }
          for(idx <- 0 until testCase){
            val zero = zeroes(idx)
            val size = 8  // equal to the weight array size
            val bitWidth = 8
            val stride = strides(idx)
            val address = addressList(idx)
            dut.io.instruction.valid #= true
            val payload = InstructionGen.loadWeightGen(zero, size - 1, stride, address, Architecture.tiny())
            dut.io.instruction.payload #= payload
            dut.clockDomain.waitSamplingWhere(dut.decode.instruction.ready.toBoolean)
            /* clear the bias using the load zero */
            insertPipe()
            val test = dut.systolicArray.array.mac.map(_.map(_.weight.toBigInt).toArray).toArray
            val unsignTest = test.map { t => t.map(v => SimTools.signClip(v, bitWidth)) }
            val testArray = loadMatrixReorder(unsignTest, bitWidth).reverse
            val step = 1 << stride
            val ref = Range(address, address + size * step, step).toArray
            if(!zero){
              assert(ref.sameElements(testArray), "load the value error!!!")
            }else{
              val ref = Array.fill(Architecture.tiny().arraySize) {Array.fill(Architecture.tiny().arraySize) {0}}.flatten
              val zeroTest = test.flatten
              assert(ref.sameElements(zeroTest), "load the all zeroes failed!!!")
            }
            assert(dut.systolicArray.array.bias.map(_.toBigInt == 0).reduce(_&&_), "the bias is not zero")
          }
        }
        init(dut)
        loadWeight(128)
        println("load instruction test success!")
        simSuccess()
    }
  }

  test("matmul"){
    SIMCFG().compile {
      val arch = Architecture.tiny()
      val dut = new Top(SInt(8 bits), arch, initContent = memoryContent)
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

        def Load(address:Int,stride:Int,size:Int) = {
          init(dut)
          println(s"========== load the memory content at address ${address} with ${size} size and ${stride} stride ======= ")
          val payload = InstructionGen.loadWeightGen(false, size - 1 , stride, address, Architecture.tiny())
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= payload
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= InstructionGen.loadWeightGen(true, 0, 0, 0, Architecture.tiny()) //load zeroes
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= InstructionGen.noOpGen(Architecture.tiny())
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          println("finish loading the weight in the systolic array...")
        }

        def Matmul(zero: Boolean, localAddress: Int, localStride: Int,
                   accumulatorAddress: Int, accumulatorStride: Int, size: Int) = {
          init(dut)
          val payload = InstructionGen.matMulGen(Architecture.tiny(), zero, localAddress, localStride, accumulatorAddress, accumulatorStride, size)._1
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= payload
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
        }

        def MatmulTest(testCase: Int) = {
          for (test <- 0 until testCase) {
            val accwriteMatrix = new ArrayBuffer[Array[Int]]()
            dut.clockDomain.onSamplings {
              if (dut.accumulatorWithALUArray.io.inputs.valid.toBoolean && dut.accumulatorWithALUArray.io.inputs.ready.toBoolean
                && dut.accumulatorWithALUArray.io.control.write.toBoolean) {
                accwriteMatrix += dut.accumulatorWithALUArray.io.inputs.payload.map(_.toInt).toArray
              }
            }
            val bitWidth = 8
            val address = Random.nextInt(512)
            val stride = Random.nextInt(4)
            val step = 1 << stride
            val loadsize = 8
            val contentArray = new ArrayBuffer[BigInt]()
            for (idx <- address until address + loadsize * step by step) {
              contentArray += memoryContent(idx)
            }
            Load(address, stride, loadsize)

            val refMatrix = MemoryContentToMatrix(contentArray.toArray, loadsize, 8)
            val refVec = refMatrix.flatten
            val loadMatrix = dut.systolicArray.array.mac.map(_.map(_.weight.toInt).toArray).toArray
            val testVec = loadMatrix.map{ t => t.map(v => SimTools.signClip(v,bitWidth))}
            val loadVec = testVec.flatten
            assert(refVec.sameElements(loadVec), "load value error!!!")
            contentArray.clear()

            val zero = Random.nextInt(10) > 5
            val localAddress = Random.nextInt(512)
            val localStride = Random.nextInt(4)
            val accumulatorAddress = Random.nextInt(512)
            val accumulatorStride = Random.nextInt(4)
            val inputsize = Random.nextInt(8) + 8
            val inputstep = 1 << localStride
            for (idx <- localAddress until localAddress + inputsize * inputstep by inputstep) {
              contentArray += memoryContent(idx)
            }
            val inputMatrix = MemoryContentToMatrix(contentArray.toArray.reverse, inputsize, 8) // input reverse and matrix * input
            Matmul(zero,localAddress, localStride, accumulatorAddress, accumulatorStride, inputsize - 1)
            if(zero) {
              val zeroref = Array.fill(Architecture.tiny().arraySize) {Array.fill(inputsize) {0}}.flatten
              val testGemmVec = accwriteMatrix.flatten
              assert(zeroref.sameElements(testGemmVec), "matmul 0 value error!!!")
            }
            else{
                /* notice the activation layout */
                val refGemm = Matrix.multiply(refMatrix.map(_.map(_.toInt)), inputMatrix.map(_.map(_.toInt)))
                val refGemmVec = SimTools.reorderMatrix(refGemm, false)
                val testGemmVec = accwriteMatrix.flatten
                assert(refGemmVec.sameElements(testGemmVec), "matmul value error!!!")
              }
            contentArray.clear()
            accwriteMatrix.clear()
          }
          println("GEMM test success!  :)")
        }
        init(dut)
        MatmulTest(256)
        simSuccess()
    }
  }

  test("data move"){
    // the data move instruction (test with the axi memory)
    SIMCFG().compile {
      val arch = Architecture.tiny()
      val dut = new Top(SInt(8 bits), arch, initContent = memoryContent)
      dut.scratchPad.io.simPublic()
      dut.accumulatorWithALUArray.io.simPublic()
      dut.accumulatorWithALUArray.accumulator.portA.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 us)
        dut.clockDomain.forkStimulus(10)
        val arch = Architecture.tiny()
        val dram0 = Axi4MemorySimV2(dut.io.weightBus,dut.clockDomain,SimConfig.axiconfig)
        val dram1 = Axi4MemorySimV2(dut.io.activationBus,dut.clockDomain,SimConfig.axiconfig)
        for(idx <- 0 until 2048){
          if(idx <= 255){ // the initial part is used to debug
            dram0.memory.writeBigInt(idx.toLong,BigInt(idx),8)
            dram1.memory.writeBigInt(idx.toLong,BigInt(idx),8)
          }else{
            val random = Random.nextInt(255)
            dram0.memory.writeBigInt(idx.toLong,BigInt(random),8)
            dram1.memory.writeBigInt(idx.toLong,BigInt(random),8)
          }
        }
        println("the dram0 and dram1 load finish!")

        dram0.start()
        dram1.start()
        def dram_to_local(num: Int, localAddress: Int, localStride: Int,
                        accumulatorAddress: Int, accumulatorStride: Int, size: Int) = {
          require(num == 0 || num == 1, "dram number should be 0 or 1")
          val step = 1 << accumulatorStride
          val behavior = if (num == 0) "dram0->memory" else "dram1->memory"
          val instruction = InstructionGen.dataMoveGen(arch, behavior, localAddress, localStride, accumulatorAddress, accumulatorStride, size)
          val buffer = new ArrayBuffer[Array[Int]]()
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= instruction._1
          dut.clockDomain.waitSamplingWhere {
            if (dut.scratchPad.io.portB.dataIn.valid.toBoolean && dut.scratchPad.io.portB.dataIn.ready.toBoolean
              && dut.scratchPad.io.portB.control.valid.toBoolean && dut.scratchPad.io.portB.control.ready.toBoolean
              && dut.scratchPad.io.portB.control.write.toBoolean) {
              buffer += dut.scratchPad.io.portB.dataIn.payload.map(_.toInt).toArray
            }
            dut.io.instruction.ready.toBoolean
          }
          val refBuffer = ArrayBuffer[Array[Int]]()
          for(address <- accumulatorAddress until accumulatorAddress + step * (size + 1) by step){
            if(num == 0){
              refBuffer += dram0.memory.readArray(address * arch.arraySize * arch.dataWidth / 8,arch.arraySize).map(_.toInt)
            }else{
              refBuffer += dram1.memory.readArray(address * arch.arraySize * arch.dataWidth / 8,arch.arraySize).map(_.toInt)
            }
          }
          assert(refBuffer.flatten == buffer.flatten,"dram to local error!!!")
          (buffer,refBuffer)
        }

        def local_to_dram(num: Int, localAddress: Int, localStride: Int,
                          accumulatorAddress: Int, accumulatorStride: Int, size: Int): ArrayBuffer[Array[Int]] = {
          require(num == 0 || num == 1, "dram number should be 0 or 1")
          val buffer = dram_to_local(0, 0, 0, 0, 0, 256)._2

          val accStep = 1 << accumulatorStride
          val localStep = 1 << localStride
          val behavior = if (num == 0) "memory->dram0" else "memory->dram1"
          val instruction = InstructionGen.dataMoveGen(arch, behavior, localAddress, localStride, accumulatorAddress, accumulatorStride, size)

          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= instruction._1
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)

          val refBuffer = ArrayBuffer[Array[Int]]()
          for (address <- localAddress until localAddress + localStep * (size + 1) by localStep) {
            refBuffer += buffer(address)
          }
          dut.io.instruction.valid #= false
          dut.clockDomain.waitSampling()
          val testBuffer = ArrayBuffer[Array[Int]]()
          for (address <- accumulatorAddress until accumulatorAddress + accStep * (size + 1) by accStep) {
            if (num == 0) {
              testBuffer += dram0.memory.readArray(address * arch.arraySize * arch.dataWidth / 8, arch.arraySize).map(_.toInt)
            } else {
              testBuffer += dram1.memory.readArray(address * arch.arraySize * arch.dataWidth / 8, arch.arraySize).map(_.toInt)
            }
          }
          assert(refBuffer.flatten == testBuffer.flatten, " local to dram error!!!")
          testBuffer
        }

        def memory_to_accumulator(localAddress: Int, localStride: Int,
                                  accumulatorAddress: Int, accumulatorStride: Int, size: Int, accumulate: Boolean): ArrayBuffer[Array[Int]] = {
          val buffer = dram_to_local(0, 0, 0, 0, 0, 256)._2
          val localStep = 1 << localStride
          val behavior = if (accumulate) "memory->accumulator(accumulate)" else "memory->accumulator"
          val instruction = InstructionGen.dataMoveGen(arch, behavior, localAddress, localStride, accumulatorAddress, accumulatorStride, size)
          val testBuffer = ArrayBuffer[Array[Int]]()
          /* the test ref is accumulator write in */
          dut.clockDomain.onSamplings {
            if (dut.accumulatorWithALUArray.accumulator.portA.dataIn.valid.toBoolean &&
              dut.accumulatorWithALUArray.accumulator.portA.dataIn.ready.toBoolean && dut.accumulatorWithALUArray.accumulator.portA.control.valid.toBoolean &&
              dut.accumulatorWithALUArray.accumulator.portA.control.ready.toBoolean && dut.accumulatorWithALUArray.accumulator.portA.control.write.toBoolean
            ) {
              testBuffer += dut.accumulatorWithALUArray.accumulator.portA.dataIn.payload.map(_.toInt).toArray
            }
          }

          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= instruction._1
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)

          if(!accumulate){
            /* the ref memory with init content */
            val refBuffer = ArrayBuffer[Array[Int]]()
            for (address <- localAddress until localAddress + localStep * (size + 1) by localStep) {
              refBuffer += buffer(address)
            }
            assert(refBuffer.flatten == testBuffer.flatten, " local to accumulator error!!!")
          } else{
            // accumulate and save into the memory
            println(testBuffer.flatten.mkString(","))
          }
          testBuffer
        }

        def accumulator_to_memory(localAddress: Int, localStride: Int,
                                  accumulatorAddress: Int, accumulatorStride: Int, size: Int) = {
          val refBuffer = memory_to_accumulator(0, 0, 0, 0, 256, false) // move data to the accumulator
          val accStep = 1 << accumulatorStride
          val behavior = "accumulator->memory"
          val instruction = InstructionGen.dataMoveGen(arch, behavior, localAddress, localStride, accumulatorAddress, accumulatorStride, size)
          val testBuffer = ArrayBuffer[Array[Int]]()
          dut.clockDomain.onSamplings {
            if (dut.scratchPad.io.portA.dataIn.valid.toBoolean && dut.scratchPad.io.portA.dataIn.ready.toBoolean
              && dut.scratchPad.io.portA.control.write.toBoolean && dut.scratchPad.io.portA.control.valid.toBoolean &&
              dut.scratchPad.io.portA.control.ready.toBoolean
            ) {
              testBuffer += dut.scratchPad.io.portA.dataIn.payload.map(_.toInt).toArray
            }
          }
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= instruction._1
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          val newrefBuffer = ArrayBuffer[Array[Int]]()
          for (address <- accumulatorAddress until accumulatorAddress + accStep * (size + 1) by accStep) {
            newrefBuffer += refBuffer(address)
          }
          assert(newrefBuffer.flatten == testBuffer.flatten,"accumulator to local error")
        }

        def dram_test(testCase:Int) = {
          dram_to_local(Random.nextInt(2), Random.nextInt(16), Random.nextInt(4), Random.nextInt(16), Random.nextInt(4), Random.nextInt(16) + 1) // dram0 -> local
          local_to_dram(Random.nextInt(2), Random.nextInt(16), Random.nextInt(4), 4096 + Random.nextInt(16), Random.nextInt(4), Random.nextInt(16) + 1)
          println("PASS DRAM Test...")
        }

        def acc_test(testCase:Int) = {
          /* first remove some data to the local */
          for(idx <- 0 until testCase){
            memory_to_accumulator(Random.nextInt(16),Random.nextInt(4),Random.nextInt(16),Random.nextInt(4),Random.nextInt(16) + 1,false)
            accumulator_to_memory(Random.nextInt(16),Random.nextInt(4),Random.nextInt(16),Random.nextInt(4),Random.nextInt(16) + 1)
          }
        }
        def acc_accumulate_test(testCase:Int) = {
          for(idx <- 0 until testCase){
            memory_to_accumulator(0, 0, 0, 0, 256, false)
            memory_to_accumulator(0,0,0,0,Random.nextInt(16),true)
          }
        }
        init(dut)
        def testCase = 32
        // a simple test about the accumulator(with accumulate)
        acc_test(testCase)
        dram_test(testCase)
        // acc_accumulate_test(testCase)
        simSuccess()
    }
  }

  test("simd"){
    SIMCFG().compile {
      val arch = Architecture.tiny()
      val dut = new Top(SInt(8 bits), arch)
      dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(1 us)
        println("random simd instruction ================>  ")
        println("for a simple way , all the simd test is based on the abs function or add function ")
        dut.clockDomain.forkStimulus(10)
        val arch = Architecture.tiny()

        // use the abs/add to test the value
        def simpleCheck(address:Int) = {
          // first move the data and then use the simd to store abs at R1
          dut.io.instruction.valid #= true
          val move_alu = InstructionGen.simdGen(arch = arch,
            readAddress= address,
            read = true,
            left = false,
            right = false,
            dest = true,
            op = 2, // move func
            writeAddress = address,
            write = false,
            accumulate = false)
          dut.io.instruction.payload #= move_alu._1 // move from the accumulator to the alu
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          println(s"move the accumulator data from address $address to the alu ")

          // write the move value to the accumulator (work for)
          val move_acc = InstructionGen.simdGen(arch = arch,
            readAddress = Random.nextInt(128),
            read = false,
            left = true,
            right = false,
            dest = false,
            op = 2, // move func
            writeAddress = Random.nextInt(128),
            write = true,
            accumulate = false)
          dut.io.instruction.payload #= move_acc._1
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          println(s"write the dest data at accumulator $address")

          val abs_alu = InstructionGen.simdGen(arch = arch,
            readAddress = address,
            read = true,
            left = false,
            right = false,
            dest = true,
            op = 11, // abs func
            writeAddress = address,
            write = false,
            accumulate = false)
            dut.io.instruction.payload #= abs_alu._1
            dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          println(s"move the accumulator data from address $address to the alu with Abs function")

          val add_alu = InstructionGen.simdGen(arch = arch,
            readAddress = address,
            read = true,
            left = true,
            right = false,
            dest = true,
            op = 8, // add it
            writeAddress = address,
            write = false,
            accumulate = false)
          dut.io.instruction.payload #= add_alu._1
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          println(s"move the accumulator data from address $address to the alu with Add function")

          // move from the accumulator to the alu and write accumulator in
          val move_alu_acc_noInput = InstructionGen.simdGen(arch = arch,
            readAddress = address,
            read = false,
            left = true,
            right = true,
            dest = true,
            op = 2, // move func
            writeAddress = address,
            write = true,
            accumulate = false)
          dut.io.instruction.payload #= move_alu_acc_noInput._1
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          println("move the alu to the acc without input")

          val move_alu_acc_Input = InstructionGen.simdGen(arch = arch,
            readAddress = address,
            read = true,
            left = false,
            right = false,
            dest = true,
            op = 2, // move func
            writeAddress = address,
            write = true,
            accumulate = false)
          dut.io.instruction.payload #= move_alu_acc_Input._1
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          println("move the alu to the acc with the input")

          // wait the accumulate in the accumulator
          val move_alu_acc_plus = InstructionGen.simdGen(arch = arch,
            readAddress = address,
            read = true,
            left = false,
            right = false,
            dest = true,
            op = 2, // move func
            writeAddress = address,
            write = true,
            accumulate = true)
          dut.io.instruction.payload #= move_alu_acc_plus._1
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
        }

        init(dut)
        for(idx <- 0 until 128){simpleCheck(Random.nextInt(128))}
        simSuccess()
    }
  }

  test("configure"){
    /* configure the regs in the top module */
    SIMCFG().compile{
      val arch = Architecture.tiny()
      val dut = new Top(SInt(8 bits), arch)
      dut.decode.io.pc.simPublic()
      dut.decode.runCycles.simPublic()
      dut.decode.interval.simPublic()
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
          update(11,1024) /* config the interval */
          println(s"current pc : ${dut.decode.io.pc.toBigInt}")
          println(s"running cycles : ${dut.decode.runCycles.toBigInt}")
          println(s"interval: : ${dut.decode.interval.toBigInt}")
          simSuccess()
        }
    }

  }
}
