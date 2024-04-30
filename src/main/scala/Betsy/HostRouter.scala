package Betsy

import spinal.core._
import spinal.lib._
import Betsy.Until._
import BetsyLibs._
/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/13      SpinalHDL Version: 1.94      **
 ** You should have received a copy of the MIT License along with this library **
 ** the data out is get the memory data to the dram **
 ** the data in is get the dram data to the memory **
 ** the kind show is data in or data out and which port to in out **
 ** Todo to be tested !!! **
 */

case class HostDataFlowControl() extends Bundle{
  val kind = UInt(2 bits)
}

object HostDataFlowControl{
  def In0 = U(0x0) /* dram 0 -> to the memory*/
  def Out0 = U(0x1) /* memory -> to the dram0 */
  def In1 = U(0x2) /* dram 1 -> to the memory */
  def Out1 = U(0x3) /* memory -> to the dram1 */
  def isDataIn(kind: UInt): Bool = {
    kind === In0 || kind === In1
  }

  def isDataOut(kind: UInt): Bool = {
    kind === Out0 || kind === Out1
  }
  def apply(kind:UInt):HostDataFlowControl = {
    val w = HostDataFlowControl()
    w.kind := kind
    w
  }
}

class HostRouter[T <: Data](val gen:HardType[T]) extends BetsyModule{
  /* the stream mux lib looks like also work (update it later...) */
  val io = new Bundle{
    val control = slave(Stream(new HostDataFlowControl())) /* with kind control */
    val dram0 = master (BetsyStreamPass(gen))
    val dram1 = master (BetsyStreamPass(gen))
    val mem = slave (BetsyStreamPass(gen))
  }

  val BetsyStreamMux = new BetsyStreamMux(gen,2)
  val BetsyStreamDeMux = new BetsyStreamDemux(gen,2)
  val isDataIn = HostDataFlowControl.isDataIn(io.control.kind)
  val isDataOut = HostDataFlowControl.isDataOut(io.control.kind)

  /* dram 0 and dram 1 -> mem.dataIn*/
  BetsyStreamMux.io.InStreams(0) <>io.dram0.dataIn
  BetsyStreamMux.io.InStreams(1) <> io.dram1.dataIn
  io.mem.dataIn <> BetsyStreamMux.io.OutStream
  BetsyStreamMux.io.sel.valid := io.control.valid && isDataIn
  BetsyStreamMux.io.sel.payload.assignFromBits(io.control.kind(1).asBits)

  /* memory.dataOut -> dram 0 and dram 1*/
  BetsyStreamDeMux.io.InStream <> io.mem.dataOut
  BetsyStreamDeMux.io.OutStreams(0) <> io.dram0.dataOut
  BetsyStreamDeMux.io.OutStreams(1) <> io.dram1.dataOut
  BetsyStreamDeMux.io.sel.valid := io.control.valid && isDataOut
  BetsyStreamDeMux.io.sel.payload.assignFromBits(io.control.kind(1).asBits)

  io.control.ready := (isDataIn && BetsyStreamMux.io.sel.ready) || (isDataOut && BetsyStreamDeMux.io.sel.ready)
}

//Todo tested ...
object HostRouter extends App{
  import spinal.core.sim._
  import scala.collection.mutable.Queue
  SIMCFG().compile {
    val dut = new HostRouter(Bits(8 bits))
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      val dram0 = new Queue[Int]()
      val dram1 = new Queue[Int]()
      val mem = new Queue[Int]()
      dut.io.control.valid #= false
      dut.io.dram0.dataIn.valid #= false
      dut.io.dram1.dataIn.valid #= false
      dut.io.mem.dataOut.valid #= false
      dut.clockDomain.waitSampling(3)

      val in0 = fork{ /* dram0 -> mem */
        while(true){
          dut.io.control.valid.randomize()
          dut.io.control.payload.kind #= 0

          dut.clockDomain.waitSampling()

        }
      }

      val in1 = fork{
        while(true){

        }
      }

      val out0 = fork{

      }

      val out1 = fork{

      }
  }
}
