package Betsy

/* the dual port memory should test with all cases */

import BetsyLibs.SIMCFG
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import SimTools._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
/* the single port write and read can not happen at same time */

class DualPortMemTest extends AnyFunSuite {

  test("memory read and write test"){
    //no masking test
    SIMCFG().compile{
      val dut = new DualPortMem(Bits(32 bits),depth = 1024,maskWidth = -1)
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)
        def depth = 1024 /* the memory depth */
        def PortInit() = {
          StreamInit(dut.io.portA.control)
          StreamInit(dut.io.portA.dataIn)
          StreamInit(dut.io.portB.control)
          StreamInit(dut.io.portB.dataIn)
          dut.clockDomain.waitSampling()
        }

        def write(depth: Int,values:Array[Int]) = {
          /* two ports write at the same time and not the same address */
          var PortAIndex = 0
          var PortBIndex = depth / 2
          while (PortAIndex < depth / 2 || PortBIndex < depth) {
            if(PortAIndex < depth / 2){
              dut.io.portA.control.valid.randomize()
              dut.io.portA.control.write #= true
              dut.io.portA.control.address #= PortAIndex
              dut.io.portA.control.size.randomize()
              dut.io.portA.dataIn.valid.randomize()
              dut.io.portA.dataIn.payload #= values(PortAIndex)
            }else{
              dut.io.portA.dataIn.valid #= false
              dut.io.portA.control.valid #= false
            }

            if(PortBIndex < depth){
              dut.io.portB.control.valid.randomize()
              dut.io.portB.control.write #= true
              dut.io.portB.control.address #= PortBIndex
              dut.io.portB.control.size.randomize()
              dut.io.portB.dataIn.valid.randomize()
              dut.io.portB.dataIn.payload #= values(PortBIndex)
            }else{
              dut.io.portB.dataIn.valid #= false
              dut.io.portB.control.valid #= false
            }

            dut.clockDomain.waitSampling()

            if(dut.io.portA.dataIn.valid.toBoolean && dut.io.portA.dataIn.ready.toBoolean &&
              dut.io.portA.control.valid.toBoolean && dut.io.portA.control.ready.toBoolean && PortAIndex < depth / 2){
              PortAIndex += 1
              // println(s"Port A write ${dut.io.portA.dataIn.payload.toBigInt} at ${dut.io.portA.control.address.toBigInt}")
            }

            if (dut.io.portB.dataIn.valid.toBoolean && dut.io.portB.dataIn.ready.toBoolean &&
              dut.io.portB.control.valid.toBoolean && dut.io.portB.control.ready.toBoolean && PortBIndex < depth) {
              PortBIndex += 1
              // println(s"Port B write ${dut.io.portB.dataIn.payload.toBigInt} at ${dut.io.portB.control.address.toBigInt}")
            }
          }
        }

        def twoPortsWrite(portAIndex:Int,portAData:Int,portBIndex:Int,portBData:Int) = {
          PortInit()
          dut.io.portA.control.valid #= true
          dut.io.portA.control.write #= true
          dut.io.portA.control.address #= portAIndex
          dut.io.portA.dataIn.valid #= true
          dut.io.portA.dataIn.payload #= portAData

          dut.io.portB.control.valid #= true
          dut.io.portB.control.write #= true
          dut.io.portB.control.address #= portBIndex
          dut.io.portB.dataIn.valid #= true
          dut.io.portB.dataIn.payload #= portBData
          dut.clockDomain.waitSampling()
        }

        def twoPortsReadWrite(portAIndex:Int,portBIndex:Int,portBData:Int) = {
          // port A read and port B write
          PortInit()
          dut.io.portA.control.valid #= true
          dut.io.portA.control.write #= false
          dut.io.portA.control.address #= portAIndex

          dut.io.portB.control.valid #= true
          dut.io.portB.control.write #= true
          dut.io.portB.control.address #= portBIndex
          dut.io.portB.dataIn.valid #= true
          dut.io.portB.dataIn.payload #= portBData

          dut.clockDomain.waitSampling()
          PortInit()
          assert(dut.io.portA.dataOut.valid.toBoolean)
          dut.io.portA.dataOut.payload.toInt
        }

        def read(depth: Int): ArrayBuffer[Int] = {
          val readBuffer = new ArrayBuffer[Int]()
          for (idx <- 0 until depth) {
            dut.io.portA.control.valid #= false
            dut.io.portA.dataIn.valid #= false
            dut.io.portB.control.valid #= true
            dut.io.portB.control.write #= false
            dut.io.portB.control.size.randomize()
            dut.io.portB.control.address #= idx
            dut.io.portB.dataIn.valid #= false
            dut.clockDomain.waitSampling(2)
            assert(dut.io.portB.dataOut.valid.toBoolean)
            readBuffer += dut.io.portB.dataOut.payload.toInt
          }
          readBuffer
        }

        def readSingle(address: Int): Int = {
          dut.io.portA.control.valid #= false
          dut.io.portA.dataIn.valid #= false
          dut.io.portB.control.valid #= true
          dut.io.portB.control.write #= false
          dut.io.portB.control.size.randomize()
          dut.io.portB.control.address #= address
          dut.io.portB.dataIn.valid #= false
          dut.clockDomain.waitSampling(2)
          assert(dut.io.portB.dataOut.valid.toBoolean)
          dut.io.portB.dataOut.payload.toInt
        }

        def commonTest() = {
          val values = Array.fill(depth){Random.nextInt(1024 * 1024)}
          write(depth, values)
          dut.clockDomain.waitSampling()
          val test = read(depth).toArray
          assert(test.sameElements(values),"common test => read and write error !!! ")
        }

        def twoPortsWriteTest() = {
          // write at the same address
          val A = Array.fill(128){Random.nextInt(1024 * 1024)}
          val B = Array.fill(128){Random.nextInt(1024 * 1024)}
          for(idx <- 0 until 128){
            val address = Random.nextInt(depth)
            twoPortsWrite(address,A(idx),address,B(idx))
            assert(readSingle(address) == B(idx),"two ports write test => read and write error !!!")
          }
        }

        def twoPortsReadWriteTest() = {
          // read and write at the same address
          write(128,(0 until 128).toArray)
          val B = Array.fill(128) {Random.nextInt(1024 * 1024)}
          for (idx <- 0 until 128) {
            val read = twoPortsReadWrite(idx,idx,B(idx))
            assert(read == idx)
            assert(readSingle(idx) == B(idx), "two ports read write test => read and write error !!!")
          }
        }

        PortInit()
        commonTest()
        twoPortsWriteTest()
        twoPortsReadWriteTest()
        simSuccess()
    }
  }

}
