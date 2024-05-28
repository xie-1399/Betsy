package BetsyLibs

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/26      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the counter can skip the value with step **
 ** Test Status : PASS :)         Version:0.1     */

import Betsy.Until.BetsyModule
import spinal.core._
import spinal.lib._
import spinal.lib.sim.StreamReadyRandomizer


class CountBy(val n:Long) extends BetsyModule{

  val io = new Bundle{
    val value = master Stream UInt(log2Up(n) bits)
    val step = in UInt(log2Up(n) bits)
    val resetValue = in Bool()
  }

  val value = Reg(UInt(log2Up(n) bits)).init(0)
  when(io.value.fire){
    val plus = value +^ io.step
    when(plus >= U(n)){
      value.clearAll()
    }.otherwise{
      value := plus.resized
    }
  }
  when(io.resetValue){
    value.clearAll()
  }

  io.value.valid := !io.resetValue
  io.value.payload := value
}

object CountBy extends App{
  /* simple test */
  import spinal.core.sim._
  import scala.util.Random
  SIMCFG().compile{
    val dut = new CountBy(1024)
    dut
  }.doSimUntilVoid{
    dut =>
      SimTimeout(100 ns)
      dut.clockDomain.forkStimulus(10)
      def testCase = 256
      StreamReadyRandomizer(dut.io.value,dut.clockDomain)
      dut.io.resetValue #= false
      dut.io.step.randomize()
      dut.clockDomain.waitSampling()

      for(idx <- 0 until testCase){
        var overflow = false
        var IndexRef = 0
        while (!overflow){
          val step = Random.nextInt(1024)
          dut.io.step #= step
          dut.io.resetValue #= false
          dut.clockDomain.waitSampling()
          if(dut.io.value.valid.toBoolean && dut.io.value.ready.toBoolean){
            assert(dut.io.value.payload.toBigInt == IndexRef,s"${dut.io.value.payload.toBigInt} not match the ${IndexRef}!!!")
            IndexRef += step
            if(IndexRef >= 1024){
              overflow = true
            }
          }
        }
        if(overflow){
          dut.io.resetValue #= true
          dut.clockDomain.waitSampling(2)
          assert(dut.io.value.payload.toInt == 0)
        }
      }
      simSuccess()
  }



}