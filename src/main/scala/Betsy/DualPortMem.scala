package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/19      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** Test Status : PASS :)         Version:0.1 **
 */

import Betsy.Until._
import spinal.core._
import spinal.lib._
import BetsyLibs._
import spinal.lib.bus.amba4.axi._

case class MemControl(depth:Long,maskWidth:Int = -1) extends Bundle with Size with Address{
  val write = Bool()
  val address = UInt(log2Up(depth) bits)
  val size = UInt(log2Up(depth) bits)
  val wmask = ifGen(maskWidth != -1){Bits(maskWidth bits)}

  def equal(other:MemControl):Bool = {
    this.write === other.write && this.address === other.address && this.size === other.size
  }

  def toAxi4(axi4Config: Axi4Config,arValid:Bool,awValid:Bool,address:UInt, size:UInt, data:Bits):Axi4 = {
    val axi4 = Axi4(axi4Config)
    axi4.ar.valid := arValid
    axi4.ar.setBurstINCR()
    axi4.ar.len := this.size.resized
    axi4.ar.size := size.resized
    axi4.ar.addr := address
    axi4.r.ready := True

    axi4.aw.valid := awValid
    axi4.aw.setBurstINCR()
    axi4.aw.len := this.size.resized
    axi4.aw.size := address.resized
    axi4.aw.addr := address

    axi4.w.valid := RegNext(axi4.aw.fire).init(False)
    axi4.w.data := data
    axi4.w.setStrb()

    val wCounter = Counter(256).init(0)
    when(axi4.w.fire){
      wCounter.increment()
    }
    val wlast = RegInit(False).setWhen(wCounter === this.size - 1).clearWhen(wCounter === this.size)
    when(wCounter === this.size){
      wCounter.clear()
    }
    axi4.w.last := wlast
    axi4.b.ready := True
    axi4
  }
}

class MemControlWithStride(depth:Long, strideDepth:Int,maskWidth:Int = -1) extends
  MemControl(depth,maskWidth) with Reverse with Stride {

  override val reverse = Bool()
  override val stride = UInt(log2Up(strideDepth) bits)

  def equal(other: MemControlWithStride): Bool = {
    this.write === other.write && this.address === other.address &&
      this.size === other.size && this.stride === other.stride && reverse === other.reverse
  }
}

object MemControlWithStride{

  def apply(depth: Long, strideDepth: Int, write: Bool, address: UInt, size: UInt, reverse: Bool, stride: UInt): MemControlWithStride = {
    val memControlWithStride = new MemControlWithStride(depth, strideDepth)
    memControlWithStride.write := write
    memControlWithStride.address := address
    memControlWithStride.size := size
    memControlWithStride.reverse := reverse
    memControlWithStride.stride := stride
    memControlWithStride
  }

}

case class Status() extends Bundle{
  val Isread = Bool()
  val Iswrite = Bool()
}

case class Port[T <: Data](gen:HardType[T], depth:Long,maskWidth:Int = -1) extends Bundle with IMasterSlave {
  val control = Stream(MemControl(depth,maskWidth))
  val dataIn = Stream(gen)
  val dataOut = Stream(gen)
  // wrote status input status
  override def asMaster(): Unit = {
    master(control,dataIn)
    slave(dataOut)
  }

  def blockPort() = {
    this.dataIn.valid := False
    this.dataIn.payload.assignFromBits(B(0).resize(this.dataIn.payload.getBitsWidth))
    this.control.valid := False
    this.control.payload.write := False
    this.control.payload.address := 0
    this.control.payload.size := 0
    if(maskWidth != -1) this.control.payload.wmask := 0
  }
}

class DualPortMem[T <: Data](gen:HardType[T], depth:Long,maskWidth:Int = -1,initContent:Array[BigInt] = null
                             ) extends BetsyModule{

  /* with a small fifo to store read data */

  val io = new Bundle{
    val portA = slave(Port(gen,depth = depth,maskWidth))
    val portB = slave(Port(gen,depth = depth,maskWidth))
  }

  def connectPorts(port: Port[T],inner: InnerPort[T]) = {
    /* no inner fifo*/
    inner.address := port.control.address
    inner.wdata := port.dataIn.payload
    if(maskWidth != -1){inner.wmask := port.control.wmask}
    inner.wen := port.control.write && port.dataIn.valid
    inner.ren := !port.control.write

    def readDelay = 1
    port.dataOut.payload := inner.rdata
    port.dataOut.valid := Delay(inner.ren && port.control.valid,readDelay).init(False)

    port.control.ready := inner.wen || (inner.ren && port.dataOut.ready)
    port.dataIn.ready := port.control.valid && port.control.write

  }

  val memoryImpl = new MemoryImpl(gen,depth,2,BlockRAM,maskWidth,initContent)  /* 2 ports block ram */

  /* connect the ports */
  connectPorts(io.portA,memoryImpl.io.Ports(0))
  connectPorts(io.portB,memoryImpl.io.Ports(1))
  val portAstatus = Status()
  portAstatus.Isread := io.portA.control.valid && !io.portA.control.write
  portAstatus.Iswrite := io.portA.control.valid && io.portA.control.write

  val portBstatus = Status()
  portBstatus.Isread := io.portB.control.valid && !io.portB.control.write
  portBstatus.Iswrite := io.portB.control.valid && io.portB.control.write
}