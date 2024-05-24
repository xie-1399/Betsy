package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/16      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 **  **
 */

import spinal.core._
import spinal.lib._
import Until._

case class SystolicArrayControl() extends Bundle with IMasterSlave {
  val load = Bool()
  val zeroes = Bool()

  override def asMaster(): Unit = {
    out(load,zeroes)
  }
}

/* add size bundle in the array */
class SystolicArrayControlWithSize(depth:Long) extends SystolicArrayControl with Size{
  override val size: UInt = UInt(log2Up(depth) bits)
}





case class AccumulatorControl(depth:Int) extends Bundle{
  val address = UInt(log2Up(depth) bits)
  val accumulate = Bool()
  val write = Bool()
}

object AccumulatorControl{
  /* init Accumulator control signals */
  def apply(depth: Int): Stream[AccumulatorControl] = {
    val control = Stream(new AccumulatorControl(depth))
    control.valid := False
    control.payload.accumulate := False
    control.payload.write := False
    control.payload.address := 0
    control
  }
}

/* the local dataflow controls the data move using the kind*/
case class LocalDataFlowControl() extends Bundle{
  val kind = UInt(4 bits) //Todo with 4 bits
}

class LocalDataFlowControlWithSize(depth:Int) extends LocalDataFlowControl with Size{
  val size = UInt(log2Up(depth) bits) /* add the size bundle to the local dataflow */
}

object LocalDataFlowControl{
  val memoryToArrayWeight = U(0x1)
  val memoryToArrayToAcc = U(0x2)
  val arrayToAcc = U(0x3)
  val accumulatorToMemory = U(0x4)
  val memoryToAccumulator = U(0x5)
  val unused = U(0x6)

  def apply(kind:UInt):LocalDataFlowControl = {
    val w = LocalDataFlowControl()
    w.kind := kind
    w
  }
}

object LocalDataFlowControlWithSize{
  def apply(depth: Int, kind: UInt, size: UInt): LocalDataFlowControlWithSize = {
    val w = new LocalDataFlowControlWithSize(depth)
    w.kind := kind
    w.size := size
    w
  }
}