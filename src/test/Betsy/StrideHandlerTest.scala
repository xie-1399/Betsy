package Betsy

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import Betsy.Until._
import BetsyLibs.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.sim.StreamReadyRandomizer
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

case class intoBundle() extends Bundle with Address with Size with Stride with Reverse{
  override val address: UInt = UInt(32 bits)
  override val size: UInt = UInt(5 bits)
  override val stride: UInt = UInt(3 bits)
  override val reverse: Bool = Bool()
  val payload:Bits = Bits(16 bits)
}

case class outputBundle() extends Bundle with Address with Size{
  override val address: UInt = UInt(32 bits)
  override val size: UInt = UInt(5 bits)
  val payload:Bits = Bits(16 bits)
}

class StrideHandlerTest extends AnyFunSuite{
  // val rtl = SpinalSystemVerilog(new StrideHandler(inGen = cloneOf(intoBundle()),outGen = cloneOf(outputBundle()),depth = 1024))

  test("stride handler"){
    SIMCFG().compile{
      val dut = new StrideHandler(inGen = cloneOf(intoBundle()),outGen = cloneOf(outputBundle()),depth = 1024)
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)
        StreamReadyRandomizer(dut.io.output,dut.clockDomain)
        def getAddress(baseAddress: BigInt, stride: Int, size: Int, reverse: Boolean): ArrayBuffer[BigInt] = {
          // Todo with the address list size
          val address = ArrayBuffer[BigInt]()
          dut.io.into.reverse.randomize()
          dut.io.into.valid #= false
          dut.io.into.stride.randomize()
          dut.io.into.size.randomize()
          dut.io.into.address.randomize()
          dut.io.into.payload.randomize()
          dut.clockDomain.waitSampling()

          dut.io.into.valid #= true
          dut.io.into.reverse #= reverse
          dut.io.into.stride #= stride
          dut.io.into.size #= size
          dut.io.into.address #= baseAddress
          dut.clockDomain.waitSamplingWhere {
            if (dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean) {
              address += dut.io.output.address.toBigInt
            }
            dut.io.into.ready.toBoolean
          }

          dut.io.into.valid #= false
          dut.clockDomain.waitSampling()
          address
        }

        val address = getAddress(1000,2,5,false)
        println(address.mkString(","))
        simSuccess()
    }
  }

}