package Betsy

import BetsyLibs.SIMCFG
import spinal.core._
import spinal.core.sim._

import scala.util.Random

/* no care about the memory usage and bus interface */

object HostRouterTest extends App{
  SIMCFG().compile {
    val dut = new HostRouter(Bits(8 bits))
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.control.valid #= false
      dut.io.dram0.dataIn.valid #= false
      dut.io.dram1.dataIn.valid #= false
      dut.io.mem.dataOut.valid #= false
      dut.clockDomain.waitSampling(3)
      var idx = 0
      def testCase = 1024
      val status = Array.fill(testCase){Random.nextInt(4)}
      val dram0 = Array.fill(testCase){Random.nextInt(128)}
      val dram1 = Array.fill(testCase){Random.nextInt(128)}
      val Mem = Array.fill(testCase){Random.nextInt(128)}

      /* test randomly from the dram to the mem*/
      while(idx < testCase - 1){
        val dram0toMem = dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean && dut.io.dram0.dataIn.valid.toBoolean && dut.io.dram0.dataIn.ready.toBoolean
        val dram1toMem = dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean && dut.io.dram1.dataIn.valid.toBoolean && dut.io.dram1.dataIn.ready.toBoolean
        val memtoDram0 = dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean && dut.io.dram0.dataOut.valid.toBoolean && dut.io.dram0.dataOut.ready.toBoolean
        val memtoDram1 = dut.io.control.valid.toBoolean && dut.io.control.ready.toBoolean && dut.io.dram1.dataOut.valid.toBoolean && dut.io.dram1.dataOut.ready.toBoolean

        dut.io.control.valid.randomize()
        dut.io.control.payload.kind #= status(idx)
        dut.io.dram0.dataIn.valid.randomize()
        dut.io.dram0.dataIn.payload #= dram0(idx)
        dut.io.dram0.dataOut.ready.randomize()
        dut.io.dram1.dataIn.valid.randomize()
        dut.io.dram1.dataIn.payload #= dram1(idx)
        dut.io.dram1.dataOut.ready.randomize()

        dut.io.mem.dataOut.valid.randomize()
        dut.io.mem.dataOut.payload #= Mem(idx)
        dut.io.mem.dataIn.ready.randomize()
        dut.clockDomain.waitSampling()
        if(dram0toMem && status(idx) == 0){
          assert(dut.io.mem.dataIn.payload.toInt == dram0(idx),"dram0 to the mem value error !!!")
          idx += 1
        }
        if(dram1toMem && status(idx) == 2){
          assert(dut.io.mem.dataIn.payload.toInt == dram1(idx),"dram1 to the mem value error !!!")
          idx += 1
        }

        if(status(idx) == 1 && memtoDram0){
          assert(dut.io.dram0.dataOut.payload.toInt == Mem(idx),"mem to the dram0 value error !!!")
          idx += 1
        }
        if(status(idx) == 3 && memtoDram1){
          assert(dut.io.dram1.dataOut.payload.toInt == Mem(idx),"mem to the dram1 value error !!!")
          idx += 1
        }
      }
      simSuccess()
  }
}
