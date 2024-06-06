package Betsy.Instruction

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/6/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 */


/**
 ** Set configuration registers
 ** Opcode = 0xF
 ** No Flags signal
 ** Argument list :
 ** OP0 is Register number
 ** OP1 Value
 ** for example :  Todo add instruction example
 **/

/* the object with box (only get and set with the Value)*/
class Box[T](value:T){
  private var _value = value
  def set(value:T) = _value = value
  def get() = _value
}
object Box{
  def apply[T](value:T) = new Box(value)
}

/* configure the register in the Betsy */

import spinal.core._
import spinal.lib._
import Betsy._

case class ConfigureArgs(registerWidth:Int,argumentsWidth:Int) extends Bundle {
  val value = Bits(argumentsWidth - registerWidth bits)
  val register = Bits(registerWidth bits)
}

object ConfigureArgs{
  val registerWidth = Box(4)
  def apply(value: Bits, register: Bits)(implicit layOut: InstructionLayOut): ConfigureArgs = {
    val arguments = ConfigureArgs(registerWidth.get(), layOut.operandsSizeBits)
    arguments.value := value.resized
    arguments.register := register.resized
    arguments
  }
}



object Configure {
  val dram0AddressOffset  = U(0x0, 4 bits)
  val dram0CacheBehaviour = U(0x1, 4 bits)
  // unused 0x02-0x03
  val dram1AddressOffset  = U(0x4, 4 bits)
  val dram1CacheBehaviour = U(0x5, 4 bits)

  // unused 0x06-0x07
  // val timeout        = 0x08
  // val tracepoint     = 0x09
  // val programCounter = 0x0a.U
  // val sampleInterval = 0x0b.U
}