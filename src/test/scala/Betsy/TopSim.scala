package Betsy

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import BetsyLibs._
import BetsyLibs.sim._
// test all kinds of models like resnet/yolo in lots of systolic array config
// first get the all instruction

class TopSim extends AnyFunSuite{
  val arch = Architecture.normal()
  val instructionFile = "/media/xxl/Betsy/src/test/scala/Betsy/binary/resnet20v2_cifar_onnx_normal.txt"
  def init(dut: Top[SInt], random:Boolean): Unit = {
    AxiInit(dut.io.activationBus)
    AxiInit(dut.io.weightBus)
    dut.io.instruction.valid #= false
    dut.io.instruction.payload.randomize()
    dut.clockDomain.waitSampling()

    val dram0 = Axi4MemorySimV2(dut.io.weightBus, dut.clockDomain, SimConfig.axiconfig)
    val dram1 = Axi4MemorySimV2(dut.io.activationBus, dut.clockDomain, SimConfig.axiconfig)
    if (random) {
      dram0.randomAlloc(0)
      dram1.randomAlloc(0)
    }
    dram0.start()
    dram1.start()
  }


  //Todo using the fix point
  test("Linear"){

  }

  test("Conv with relu"){

  }

  // whole network
  test("resnet20_cifar") {
    SIMCFG().compile {
      val dut = new Top(SInt(16 bits), arch = arch) // 64 * 64 and 16 bits
      dut
    }.doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
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
