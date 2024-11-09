package Betsy

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import BetsyLibs._
import BetsyLibs.sim._

import java.io.File
import scala.collection.mutable.ArrayBuffer

class TopSim extends AnyFunSuite{
  val arch = Architecture.normal()

  def init[T <: Data with Num[T]](dut: Top[T], dram0:Axi4MemorySimV2, dram1:Axi4MemorySimV2, random:Boolean = false, path:String=""){
    AxiInit(dut.io.activationBus)
    AxiInit(dut.io.weightBus)
    dut.io.instruction.valid #= false
    dut.io.instruction.payload.randomize()
    dut.clockDomain.waitSampling()
    // load the weight data into the dram1 , load the activation data into the dram0
    dram1.memory.loadBinary(0, path)
    // val numInputs = Array.fill(64){1}.map(_.toByte)
    val numInputs = (0 to 63).map(_.toByte).toArray
    val zeroArray = new Array[Int](64).map(_.toByte)
    val inputs = new Array[Byte](128)
    numInputs.zip(zeroArray).zipWithIndex.foreach(tuple => {
      inputs(2 * tuple._2) = tuple._1._2
      inputs(2 * tuple._2 + 1) = tuple._1._1
    })
    dram0.memory.writeArray(0,inputs.toArray)
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
        val dram0 = Axi4MemorySimV2(dut.io.activationBus, dut.clockDomain, SimConfig.axiconfig)
        val dram1 = Axi4MemorySimV2(dut.io.weightBus, dut.clockDomain, SimConfig.axiconfig)

        val rootDirectory = new File(".").getCanonicalPath
        val instructionFile = rootDirectory + "/temp/Linear_64_256_10_onnx_normal.txt"
        val instructionBuffer: Array[BigInt] = Logger.readFile(instructionFile).map(BigInt(_, 2)).toArray
        println(instructionBuffer.length)
        init(dut,dram0 = dram0, dram1 = dram1, path = rootDirectory + "/temp/Linear_64_256_10_onnx_normal.tdata")
        var pc = 0
        while (pc < instructionBuffer.length) {
          dut.io.instruction.valid #= true
          dut.io.instruction.payload #= instructionBuffer(pc)
          dut.clockDomain.waitSamplingWhere(dut.io.instruction.ready.toBoolean)
          pc += 1
          println(s"current pc : $pc")
        }
        // read the finally results
        val results = dram0.memory.readArray(0.toLong, 20.toLong)
        val res = ArrayBuffer[Double]()
        for(idx <- 0 until 10){
          val head = if(results(idx * 2 + 1) >= 0) "0"*(8 - results(idx * 2 + 1).toBinaryString.length) + results(idx * 2 + 1).toBinaryString else results(idx * 2 + 1).toBinaryString.takeRight(8)
          val tail = if(results(idx * 2) >= 0) "0"*(8 - results(idx * 2).toBinaryString.length) + results(idx * 2).toBinaryString else results(idx * 2).toBinaryString.takeRight(8)
          res.append(SimTools.convertToFixedPoint(head+tail,7,8))
        }
        val refs = Logger.readFile(rootDirectory + "/temp/linear_result.txt").toArray.map(_.toDouble)
        refs.foreach(r => print(r.toString + " "))
        println()
        res.foreach(r => print(r.toString + " "))
        println()
        val res_array = res.toArray
        val errors = refs.zip(res_array).map { case (a, b) => Math.abs(a - b) }
        val averageError = errors.sum / errors.length
        println(s"average Error: $averageError")
        simSuccess()
    }
  }

  test("Convolution"){

  }

  // whole network
  test("resnet20") {
    SIMCFG().compile {
      val dut = new Top(AFix(7 exp, -8 exp, true), arch = arch) // 64 * 64 and 16 bits
      dut
    }.doSimUntilVoid {
      dut =>
        simSuccess()
    }
  }
}
