package Betsy

import org.scalatest.funsuite.AnyFunSuite
import BetsyLibs.SIMCFG
import spinal.core._
import spinal.core.sim._
import SimTools._
import spinal.lib.sim.StreamReadyRandomizer

class AccumulateWithALUArrayTest extends AnyFunSuite{

  test("accumulate it"){

    /* read about it and simple check about it */
    SIMCFG().compile{
      val dut = new AccumulatorWithALUArray(SInt(8 bits),Architecture.tiny())
      dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(1 us)
        dut.clockDomain.forkStimulus(10)
        def init() = {
          StreamInit(dut.io.inputs)
          StreamInit(dut.io.control)
          dut.clockDomain.waitSampling()
        }

        def read(address: Int): Array[Int] = {
          init()
          dut.io.control.valid #= true
          dut.io.control.payload.read #= true
          dut.io.control.payload.write #= false
          dut.io.control.payload.accumulate #= false
          dut.io.control.payload.readAddress #= address
          dut.io.control.payload.writeAddress #= 0
          dut.io.control.payload.SIMDInstruction.op #= 0  // noOp
          dut.io.outputs.ready #= true
          dut.clockDomain.waitSamplingWhere(dut.io.outputs.valid.toBoolean)
          dut.io.outputs.payload.map(_.toInt).toArray
        }

        def write(address:Int, data: Array[Int]) = {
          init()
          dut.io.control.valid #= true
          dut.io.control.payload.read #= false
          dut.io.control.payload.write #= true
          dut.io.control.payload.accumulate #= false
          dut.io.control.payload.readAddress #= 0
          dut.io.control.payload.writeAddress #= address
          dut.io.control.payload.SIMDInstruction.op #= 0 // noOp

          dut.io.inputs.valid #= true
          dut.io.inputs.payload.zipWithIndex.map(v => v._1 #= data(v._2))
          dut.clockDomain.waitSamplingWhere(dut.io.control.ready.toBoolean)

          dut.io.control.valid #= false
          dut.clockDomain.waitSampling()
        }

        def accumulate(address:Int, data: Array[Int]) = {
          init()
          dut.io.control.valid #= true
          dut.io.control.payload.read #= false
          dut.io.control.payload.write #= true
          dut.io.control.payload.accumulate #= true
          dut.io.control.payload.readAddress #= 0
          dut.io.control.payload.writeAddress #= address
          dut.io.control.payload.SIMDInstruction.op #= 0 // noOp

          dut.io.inputs.valid #= true
          dut.io.inputs.payload.zipWithIndex.map(v => v._1 #= data(v._2))
          dut.clockDomain.waitSamplingWhere(dut.io.control.ready.toBoolean)

          dut.io.control.valid #= false
          dut.clockDomain.waitSampling()
        }
        init()
        write(0,Array(0,1,2,3,4,5,6,7))
        println(read(0).mkString(","))

        accumulate(0,Array(0,1,2,3,4,5,6,7))
        println(read(0).mkString(","))
        simSuccess()
    }
  }

}
