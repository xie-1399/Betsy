package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/16      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 **  **
 */

import Betsy.LocalDataFlowControl._
import spinal.core._
import spinal.lib._
import Until._

/*=======================Systolic Array Control========================== */
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

object SystolicArrayControlWithSize{
  def apply(depth: Long, load: Bool, zeroes: Bool, size: UInt): SystolicArrayControlWithSize = {
    val systolicArrayControlWithSize = new SystolicArrayControlWithSize(depth)
    systolicArrayControlWithSize.size := size
    systolicArrayControlWithSize.load := load
    systolicArrayControlWithSize.zeroes := zeroes
    systolicArrayControlWithSize
  }
}

/*=======================Accumulator Control========================== */
class AccumulatorControl(depth:Long) extends Bundle{
  val address = UInt(log2Up(depth) bits)
  val accumulate = Bool()
  val write = Bool()
}

object AccumulatorControl{
  /* init Accumulator control signals */
  def apply(depth: Long): Stream[AccumulatorControl] = {
    val control = Stream(new AccumulatorControl(depth))
    control.valid := False
    control.payload.accumulate := False
    control.payload.write := False
    control.payload.address := 0
    control
  }
}

// with the alu control
case class AccumulatorWithALUArrayControl(layOut: InstructionLayOut) extends Bundle{
  val SIMDInstruction = new SIMDInstruction(layOut)
  val readAddress = UInt(log2Up(layOut.arch.accumulatorDepth) bits)
  val writeAddress = UInt(log2Up(layOut.arch.accumulatorDepth) bits)
  val accumulate = Bool()
  val write = Bool()
  val read = Bool()
}

object AccumulatorWithALUArrayControl{
  def apply(layOut: InstructionLayOut, SIMDInstruction: SIMDInstruction, readAddress: UInt, writeAddress: UInt,
            read: Bool, write: Bool, accumulate: Bool): AccumulatorWithALUArrayControl = {
    val control = AccumulatorWithALUArrayControl(layOut)
    control.SIMDInstruction <> SIMDInstruction
    control.readAddress := readAddress
    control.writeAddress := writeAddress
    control.read := read
    control.write := write
    control.accumulate := accumulate
    control
  }

  def read(address: UInt)(implicit layOut: InstructionLayOut): AccumulatorWithALUArrayControl = {
    apply(layOut, SIMDInstruction.noOp(), address, U(0).resized, True, False, False)
  }

  def write(address: UInt, accumulate: Bool = False)(implicit layOut: InstructionLayOut): AccumulatorWithALUArrayControl = {
    apply(layOut, SIMDInstruction.noOp(), U(0).resized, address, False, True, accumulate)
  }
}

case class AccumulatorMemControl(layOut: InstructionLayOut) extends Bundle with Address with Size{
  val instruction = new SIMDInstruction(layOut)
  val address = UInt(log2Up(layOut.arch.accumulatorDepth) bits) /* read address */
  val altAddress = UInt(log2Up(layOut.arch.accumulatorDepth) bits)
  val read = Bool()
  val write = Bool()
  val accumulate = Bool()
  val size = UInt(log2Up(layOut.arch.accumulatorDepth) bits)

  /* let the memory operation to the accumulator operation */
  def toAccumulatorWithALUArrayControl(arch: Architecture): AccumulatorWithALUArrayControl = {
    val layOut = InstructionLayOut(arch)
    val w = new AccumulatorWithALUArrayControl(layOut)
    val isMemControl = instruction.op === ALUOp.NoOp
    w.SIMDInstruction := instruction
    w.read := read
    w.write := write
    w.accumulate := accumulate
    when(isMemControl) {
      when(read) {
        w.readAddress := address
        w.writeAddress := altAddress
      }.otherwise {
        when(write) {
          w.readAddress := altAddress
          w.writeAddress := address
        }.otherwise {
          w.readAddress := address
          w.writeAddress := altAddress
        }
      }
    }.otherwise {
      w.readAddress := address
      w.writeAddress := altAddress
    }
    w
  }
}

class AccumulatorMemControlWithSizeWithStride(layOut: InstructionLayOut) extends AccumulatorMemControl(layOut)
  with Stride
  with Reverse {
  val stride  = UInt(log2Up(layOut.arch.stride1Depth) bits)
  val reverse = Bool()
}

object AccumulatorMemControlWithSizeWithStride{

  def apply(layOut: InstructionLayOut, instruction: SIMDInstruction, address: UInt, altAddress: UInt, read: Bool,
            write: Bool, accumulate: Bool, size: UInt, stride: UInt, reverse: Bool): AccumulatorMemControlWithSizeWithStride = {
    val accumulatorMemControlWithSizeWithStride = new AccumulatorMemControlWithSizeWithStride(layOut)
    accumulatorMemControlWithSizeWithStride.instruction := instruction
    accumulatorMemControlWithSizeWithStride.address := address
    accumulatorMemControlWithSizeWithStride.altAddress := altAddress
    accumulatorMemControlWithSizeWithStride.read := read
    accumulatorMemControlWithSizeWithStride.write := write
    accumulatorMemControlWithSizeWithStride.accumulate := accumulate
    accumulatorMemControlWithSizeWithStride.size := size
    accumulatorMemControlWithSizeWithStride.stride := stride
    accumulatorMemControlWithSizeWithStride.reverse := reverse
    accumulatorMemControlWithSizeWithStride
  }

}


/*=======================Host Router Control========================== */
class HostDataFlowControl() extends Bundle{
  val kind = UInt(2 bits)
}

/* control with size and host dataflow kind */
case class HostDataFlowControlWithSize(depth:Long) extends HostDataFlowControl with Size{
  val size = UInt(log2Up(depth) bits)
}

object HostDataFlowControl{
  def In0: UInt = U(0x0)
  /* dram 0 -> to the memory*/
  def Out0: UInt = U(0x1)
  /* memory -> to the dram0 */
  def In1: UInt = U(0x2)
  /* dram 1 -> to the memory */
  def Out1: UInt = U(0x3) /* memory -> to the dram1 */
  def isDataIn(kind: UInt): Bool = {
    kind === In0 || kind === In1
  }

  def isDataOut(kind: UInt): Bool = {
    kind === Out0 || kind === Out1
  }
  def apply(kind:UInt):HostDataFlowControl = {
    val w = new HostDataFlowControl()
    w.kind := kind
    w
  }
}

object HostDataFlowControlWithSize{
  def apply(depth: Long, kind: UInt, size: UInt): HostDataFlowControlWithSize = {
    val hostDataFlowControlWithSize = HostDataFlowControlWithSize(depth)
    hostDataFlowControlWithSize.kind := kind.resized
    hostDataFlowControlWithSize.size := size.resized
    hostDataFlowControlWithSize
  }
}
/*=======================Local DataFlow Control========================== */
/* the local dataflow controls the data move using the kind*/
case class LocalDataFlowControl() extends Bundle{
  val sel = UInt(log2Up(locaDataFlowNums) bits)
}

class LocalDataFlowControlWithSize(depth:Long) extends LocalDataFlowControl() with Size{
  val size = UInt(log2Up(depth) bits) /* add the size bundle to the local dataflow */
}

/* select the dataflow and control with size */
case class DataFlowSel(num:Int) extends Bundle{
  val sel = UInt(log2Up(num) bits)
}
class DataFlowSelWithSize(num:Int,depth:Long) extends DataFlowSel(num) with Size{
  override val size: UInt = UInt(log2Up(depth) bits)
}

object LocalDataFlowControl{
  // update the local dataflow control
  def memoryToArrayWeight: UInt = U(0x1)
  def memoryToArrayToAcc: UInt = U(0x2)
  def arrayToAcc: UInt = U(0x3)
  def accumulatorToMemory: UInt = U(0x4)
  def memoryToAccumulator: UInt = U(0x5)
  def unused: UInt = U(0x6)
  def localDataFlows: Array[UInt] = Array(memoryToArrayWeight, memoryToArrayToAcc, arrayToAcc, accumulatorToMemory, memoryToAccumulator, unused)
  def locaDataFlowNums = localDataFlows.length

  def apply(sel:UInt):LocalDataFlowControl = {
    val w = LocalDataFlowControl()
    w.sel := sel
    w
  }
}

object LocalDataFlowControlWithSize{
  def apply(depth: Long, sel: UInt, size: UInt): LocalDataFlowControlWithSize = {
    val w = new LocalDataFlowControlWithSize(depth)
    w.sel := sel
    w.size := size
    w
  }
}

object DataFlowSel{
  def apply(num: Int, sel: UInt): DataFlowSel = {
    val select = new DataFlowSel(num)
    select.sel := sel
    select
  }
}

object DataFlowSelWithSize{
  def apply(num: Int, depth: Long, sel: UInt, size: UInt): DataFlowSelWithSize = {
    val select = new DataFlowSelWithSize(num, depth)
    select.sel := sel
    select.size := size
    select
  }
}