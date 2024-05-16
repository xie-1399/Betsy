package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/19      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Dual Port memory unit simple test passed !!! **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._
import BetsyLibs._

//Todo with the size?

case class MemControl(depth:Int,maskWidth:Int = -1) extends Bundle with Size{
  val write = Bool()
  val address = UInt(log2Up(depth) bits)
  val size = UInt(log2Up(depth) bits)
  val wmask = ifGen(maskWidth != -1){Bits(maskWidth bits)}
}

case class Port[T <: Data](gen:HardType[T], depth:Int,maskWidth:Int = -1) extends Bundle with IMasterSlave {
  val control = Stream(MemControl(depth,maskWidth))
  val dataIn = Stream(gen)
  val dataOut = Stream(gen)
  // wrote status input status
  override def asMaster(): Unit = {
    master(control,dataIn)
    slave(dataOut)
  }
}

class DualPortMem[T <: Data](gen:HardType[T], depth:Int,maskWidth:Int = -1
                             ) extends BetsyModule{
  val io = new Bundle{
    val portA = slave(Port(gen,depth = depth,maskWidth))
    val portB = slave(Port(gen,depth = depth,maskWidth))
  }

  def connectPorts(port: Port[T],inner: InnerPort[T]) = {
    /* no inner fifo*/
    inner.address := port.control.address
    inner.wdata := port.dataIn.payload
    if(maskWidth != -1){inner.wmask := port.control.wmask}
    inner.wen := port.control.write && port.dataIn.valid && port.control.valid
    inner.ren := !port.control.write && port.control.valid

    port.dataOut.payload := inner.rdata
    port.dataOut.valid := RegNext(!port.control.write && port.control.valid).init(False)

    port.control.ready := inner.wen || inner.ren
    port.dataIn.ready := port.control.valid && port.control.write
  }

  val memoryImpl = new MemoryImpl(gen,depth,2,BlockRAM,maskWidth)  /* 2 ports block ram */

  /* connect the ports */
  connectPorts(io.portA,memoryImpl.io.Ports(0))
  connectPorts(io.portB,memoryImpl.io.Ports(1))
}

object DualPortMem extends App{
  SpinalSystemVerilog(new DualPortMem(UInt(8 bits),1024))

  import spinal.core.sim._
  SIMCFG().compile{
   val dut = new DualPortMem(UInt(16 bits),1024)
   dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.portA.control.valid #= false
      dut.io.portA.dataIn.valid #= false
      dut.io.portB.control.valid #= false
      dut.io.portB.dataIn.valid #= false
      dut.clockDomain.waitSampling(2)

      def write(depth: Int) = {
        var num = 0
        while (num < depth) {
          dut.io.portA.control.valid.randomize()
          dut.io.portA.control.write #= true
          dut.io.portA.control.address #= num
          dut.io.portA.control.size.randomize()
          dut.io.portA.dataIn.valid.randomize()
          dut.io.portA.dataIn.payload #= num
          dut.io.portB.control.valid #= false
          dut.clockDomain.waitSampling()
          if (dut.io.portA.dataIn.valid.toBoolean && dut.io.portA.dataIn.ready.toBoolean && dut.io.portA.control.valid.toBoolean && dut.io.portA.control.ready.toBoolean) {
            num += 1
            println(s"write the ${dut.io.portA.dataIn.payload.toBigInt} at ${dut.io.portA.control.address.toBigInt}...")
          }
        }
      }

      def read(depth: Int) = {
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
          assert(dut.io.portB.dataOut.payload.toInt == idx, s"${dut.io.portB.dataOut.payload.toInt} =/= ${idx}")
        }
        println("read test Ready ... ")
      }

      write(1024)
      read(1024)
      simSuccess()
  }

}