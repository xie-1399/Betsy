package Betsy

import Betsy.Until._
import BetsyLibs.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.StreamReadyRandomizer

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

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

case class intoSize() extends Bundle with Size {
  override val size: UInt = UInt(4 bits)
  val payload: Bits = Bits(16 bits)
}

case class outputSize() extends Bundle {
  val payload: Bits = Bits(16 bits)
}

class StrideHandlerTest extends AnyFunSuite{
  // val rtl = SpinalSystemVerilog(new StrideHandler(inGen = cloneOf(intoBundle()),outGen = cloneOf(outputBundle()),depth = 1024))
  test("stride handler"){
    SIMCFG().compile{
      val dut = new StrideHandler(inGen = cloneOf(intoBundle()),outGen = cloneOf(outputBundle()),depth = 1024)
      dut
    }.doSimUntilVoid{
      dut =>
        SimTimeout(10 us)
        dut.clockDomain.forkStimulus(10)
        StreamReadyRandomizer(dut.io.output,dut.clockDomain)
        def getAddress(baseAddress: BigInt, stride: Int, size: Int, reverse: Boolean): ArrayBuffer[BigInt] = {
          val address = ArrayBuffer[BigInt]()
          address.clear()
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

          if(size == 0){
            dut.clockDomain.waitSamplingWhere(dut.io.into.ready.toBoolean)
            address += dut.io.output.address.toBigInt
            assert(dut.io.output.address.toBigInt == baseAddress)
            assert(dut.io.output.size.toBigInt == size)
          }else{
            while(!(dut.io.into.ready.toBoolean && dut.io.into.valid.toBoolean)){
              dut.clockDomain.waitSampling()
              if (dut.io.output.valid.toBoolean && dut.io.output.ready.toBoolean && !(dut.io.into.ready.toBoolean && dut.io.into.valid.toBoolean)) {
                address += dut.io.output.address.toBigInt
              }
            }
          }
          dut.io.into.valid #= false
          dut.clockDomain.waitSampling()
          address
        }
        def testCase = 1024
        for (idx <- 0 until testCase){
          val baseAddr = Random.nextInt(1024 * 1024)
          val size = Random.nextInt(15) + 1
          getAddress(baseAddr,0,0,false) // when the size is 0 and stride should be also 0
          val address = getAddress(baseAddr,stride = 2,size = size,reverse = false)
          val ref = (baseAddr until baseAddr + 4 * size by 4).toArray
          /* println(s"BASE ADDR:  ${baseAddr}")
          println(address.mkString(","))
          println(ref.mkString(",")) */
          assert(address.sameElements(ref))
        }
        simSuccess()
    }
  }

  /* the sizeHandler is the simple Stride Handler */
  test("size handler") {
    SIMCFG().compile {
      val dut = new SizeHandler(inGen = cloneOf(intoSize()), outGen = cloneOf(outputSize()), depth = 1024)
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 us)
        dut.clockDomain.forkStimulus(10)
        StreamReadyRandomizer(dut.io.output,dut.clockDomain)
        dut.io.into.valid #= false
        dut.io.into.payload.randomize()
        dut.clockDomain.waitSampling()

        def testCase = 1024
        val sizes = Array.fill(testCase){Random.nextInt(8)}
        for(idx <- 0 until testCase){
          var cycles = 0
          dut.io.into.valid #= true
          dut.io.into.size #= sizes(idx)
          dut.clockDomain.waitSamplingWhere{
            if(dut.io.output.ready.toBoolean && dut.io.output.valid.toBoolean && !(dut.io.into.ready.toBoolean && dut.io.into.valid.toBoolean)){
              cycles += 1
            }
            dut.io.into.ready.toBoolean && dut.io.into.valid.toBoolean
          }
          assert(cycles == sizes(idx),s"${cycles} not match the ${sizes(idx)}!!!")
          dut.io.into.valid #= false
          dut.clockDomain.waitSampling()
        }
        simSuccess()
    }
  }

}