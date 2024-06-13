package Betsy

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import BetsyLibs._
import spinal.lib.sim.StreamReadyRandomizer
import scala.util.Random
/* test for the accumulator and alu module together */

class AccumulatorWithALUTest extends AnyFunSuite{

  test("accumulator it"){
    SIMCFG().compile{
      val dut = new AccumulatorWithALUArray(SInt(16 bits),Architecture.tiny())
      dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(1 us)
        dut.clockDomain.forkStimulus(10)
        StreamReadyRandomizer(dut.io.outputs,dut.clockDomain)
        def init() = {
          dut.io.control.valid #= false
          dut.io.control.payload.randomize()
          dut.io.inputs.valid #= false
          dut.io.inputs.payload.randomize()
          dut.clockDomain.waitSampling()
        }

        /* first test write the value into the accumulator*/
        def writeAcc(address:Int,value:Array[Int]) = {
          while(!(dut.io.inputs.valid.toBoolean && dut.io.inputs.ready.toBoolean && dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean)){
            dut.io.inputs.valid.randomize()
            dut.io.inputs.payload.zipWithIndex.map(input => input._1 #= value(input._2))
            dut.io.control.valid.randomize()
            dut.io.control.payload.write #= true
            dut.io.control.payload.accumulate #= false
            dut.io.control.writeAddress #= address
            dut.io.control.read #= false
            dut.io.control.SIMDInstruction.op #= 0  // noOp
          }
          println(s"write ${value.mkString(",")} at address ${address} ")
        }
        init()
        writeAcc(Random.nextInt(1024),Array.fill(8){Random.nextInt(1024)})
        simSuccess()
    }
  }

}
