package BetsyLibs

import spinal.core._

import scala.util.Random

//Todo with (two ports write at the same time on the same address || (read and write at the same time on the same address))

object MemoryImplTest extends App{
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new MemoryImpl(Bits(32 bits),1024,2,BlockRAM) /* two ports ram and seems like read first */
    dut
  }.doSimUntilVoid{
    dut =>
      // no mask test(simple read and write is ready)
      dut.clockDomain.forkStimulus(10)
      val port0 = dut.io.Ports(0)
      val port1 = dut.io.Ports(1)
      port0.wen #= false
      port0.ren #= false
      port1.wen #= false
      port1.ren #= false
      dut.clockDomain.waitSampling()

      def write(depth:Int) = {
        for(idx <- 0 until depth){
          port0.wen #= true
          port0.ren #= false
          port0.address #= idx
          port0.wdata #= idx
          dut.clockDomain.waitSampling()
        }
        dut.clockDomain.waitSampling()
        println("write the data down...")
      }

      def read(depth: Int) = {
        for (idx <- 0 until depth) {
          port0.wen #= false
          port1.ren #= true
          port1.address #= idx
          dut.clockDomain.waitSampling(2)
          assert(port1.rdata.toInt == idx, s"${port1.rdata.toInt} =/= ${idx}")
        }
        println("read test Ready ... ")
      }

      def readAndWrite(address:Int) = {
        port0.wen #= true
        port0.ren #= true
        port1.wen #= false
        port1.ren #= false
        port0.address #= address
        port0.wdata.randomize()
        dut.clockDomain.waitSampling()
        val value = Random.nextInt(1024)
        port0.wen #= true
        port0.ren #= false
        port0.wdata #= value
        dut.clockDomain.waitSampling()
        assert(port0.rdata.toBigInt == address)
        port0.wen #= false
        port0.ren #= true
        port0.address #= address
        dut.clockDomain.waitSampling(2)
        assert(port0.rdata.toBigInt == value)
      }

      write(1024)
      read(1024)
      for(idx <- 0 until 1024) readAndWrite(idx)

      simSuccess()
  }
}
