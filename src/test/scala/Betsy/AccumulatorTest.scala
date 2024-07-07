package Betsy

import Betsy.SimTools._
import BetsyLibs.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.StreamReadyRandomizer

import scala.util.Random
/* test 3 paths in the accumulator */

class AccumulatorTest extends AnyFunSuite{

  test("Accumulate It"){

    SIMCFG().compile{
      val dut = new Accumulator(SInt(32 bits),4,512)
      dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(10 us)
        dut.clockDomain.forkStimulus(10)
        StreamReadyRandomizer(dut.io.dataOut,dut.clockDomain)
        def init() = {
          StreamInit(dut.io.dataIn)
          StreamInit(dut.io.control)
          dut.clockDomain.waitSampling()
        }

        //just write
        def write(address:Int,data:Array[Int]) = {
          while(!(dut.io.control.ready.toBoolean && dut.io.control.valid.toBoolean
            && dut.io.dataIn.valid.toBoolean && dut.io.dataIn.ready.toBoolean)){
            dut.io.control.valid.randomize()
            dut.io.dataIn.valid.randomize()
            dut.io.dataIn.payload.zipWithIndex.map(i => i._1 #= data(i._2))
            dut.io.control.payload.accumulate #= false
            dut.io.control.payload.write #= true
            dut.io.control.payload.address #= address
            dut.clockDomain.waitSampling()
          }
        }
        //just read from portA
        def read(address: Int): Array[Int] = {
          while (!(dut.io.control.ready.toBoolean && !dut.io.control.payload.write.toBoolean)) {
            StreamInit(dut.io.dataIn)
            dut.io.control.valid.randomize()
            dut.io.control.payload.accumulate #= false
            dut.io.control.payload.write #= false
            dut.io.control.payload.address #= address
            dut.clockDomain.waitSampling()
          }
          assert(dut.io.dataOut.valid.toBoolean && dut.io.dataOut.ready.toBoolean, "read ready signal error !!!")
          dut.io.dataOut.payload.map(_.toInt).toArray
        }

        // accumulate it(read and write at the same address)
        def accumulate(address:Int,data:Array[Int]) = {
          while (!(dut.io.control.ready.toBoolean && dut.io.control.payload.accumulate.toBoolean)) {
            dut.io.control.valid.randomize()
            dut.io.control.payload.accumulate #= true
            dut.io.control.payload.write #= true
            dut.io.control.payload.address #= address
            dut.io.dataIn.valid.randomize()
            dut.io.dataIn.payload.zipWithIndex.map(i => i._1 #= data(i._2))
            dut.clockDomain.waitSampling()
          }
        }

        init()
        def testCase = 512
        val arrays = Array.fill(testCase){Array.fill(4)(Random.nextInt(1024 * 1024))}
        val accs = Array.fill(testCase){Array.fill(4){Random.nextInt(1024)}}
        for(idx <- 0 until testCase){
          write(idx,arrays(idx))
          assert(arrays(idx).sameElements(read(idx)),"read and write is not match!!!")
          accumulate(idx,accs(idx))
          val test = read(idx)
          val ref = vectorAdd(accs(idx),arrays(idx))
          assert(ref.sameElements(test))
        }
        simSuccess()
    }


  }

}
