package Betsy

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import BetsyLibs._
import BetsyLibs.sim._
import scala.collection.mutable.ArrayBuffer
// test all kinds of models like resnet/yolo in lots of systolic array config
// first get the all instruction

class TopSim extends AnyFunSuite{
  val arch = Architecture.normal()
  def init[T <: Data with Num[T]](dut: Top[T], random:Boolean = false): Unit = {
    AxiInit(dut.io.activationBus)
    AxiInit(dut.io.weightBus)
    dut.io.instruction.valid #= false
    dut.io.instruction.payload.randomize()
    dut.clockDomain.waitSampling()

    val dram0 = Axi4MemorySimV2(dut.io.weightBus, dut.clockDomain, SimConfig.axiconfig)
    val dram1 = Axi4MemorySimV2(dut.io.activationBus, dut.clockDomain, SimConfig.axiconfig)

    // load the weight data into the dram1 , load the activation data into the dram0
    dram1.memory.loadBinary(0, "/home/zhangtr/NPU/Betsy/software/src/tensil/tools/gen/Linear_64_256_10_op10_onnx_normal.tdata")

    // input
//    for(idx <- 0 to 63){
//
//    }
    val numInputs = ArrayBuffer.range(63, 0, -1).map(_.toByte).toArray

//    val numInputs = Array.fill(64)(1).map(_.toByte)
    val zeroArray = new Array[Int](64).map(_.toByte)
    val inputs = new Array[Byte](128)
    numInputs.zip(zeroArray).zipWithIndex.foreach(tuple => {
      inputs(2 * tuple._2) = tuple._1._2
      inputs(2 * tuple._2 + 1) = tuple._1._1
    })

    println(inputs)
    dram0.memory.writeArray(0,inputs.toArray)
    // dram0.memory.loadBinary(0,"")

    println("load the memory finish!")
    if (random) {
      dram0.randomAlloc(0)
      dram1.randomAlloc(0)
    }
    dram0.start()
    dram1.start()
  }

  test("Linear"){
    SIMCFG().compile {
      val dut = new Top(AFix(7 exp, -8 exp, true), arch = arch) // 64 * 64 and 16 bits
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        val instructionFile = "/home/zhangtr/NPU/Betsy/src/test/scala/Betsy/binary/Linear_64_256_10_op10_onnx_normal.txt"
        val instructionBuffer: Array[BigInt] = Logger.readFile(instructionFile).map(BigInt(_, 2)).toArray
        println(instructionBuffer.length)
        init(dut)
        var pc = 0
        while (pc < instructionBuffer.length) {
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= instructionBuffer(pc)
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          pc += 1
          println(s"current pc : $pc")
        }
        simSuccess()
    }
  }

  test("Conv with relu"){

  }

  // whole network
  test("resnet20_cifar") {
    SIMCFG().compile {
      val dut = new Top(AFix(7 exp, -8 exp, true), arch = arch) // 64 * 64 and 16 bits
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        val instructionFile = "/home/zhangtr/NPU/Betsy/src/test/scala/Betsy/binary/resnet20v2_cifar_onnx_normal.txt"
        val instructionBuffer: Array[BigInt] = Logger.readFile(instructionFile).map(BigInt(_, 2)).toArray
        println(instructionBuffer.length)
        init(dut, random = true)
        var pc = 0
        while(pc < instructionBuffer.length){
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= instructionBuffer(pc)
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          pc += 1
          println(s"current pc : $pc")
        }
        simSuccess()
    }
  }
}
