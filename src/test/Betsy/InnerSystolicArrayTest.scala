package Betsy

import BetsyLibs.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class InnerSystolicArrayTest extends AnyFunSuite{

  /* Todo random array size (notice the difference between def and val)*/
  val ArrayHeight =  4 //Random.nextInt(16) + 1
  val ArrayWidth = 4 // Random.nextInt(16) + 1

  def reorderMatrix(array:Array[Array[Int]]) = {
    val rows = array.length
    val cols = array(0).length
    val buffer = new ArrayBuffer[Int]()
    for( j <- cols - 1 to 0 by -1 ){
      for( i <- 0 until rows ){
        buffer += array(i)(j)
      }
    }
    buffer.toArray
  }

  def loadWeight(dut:InnerSystolicArray[SInt],weights:Array[Array[Int]]) = {
    dut.io.load #= true
    dut.io.input.randomize()
    for(i <- 0 until ArrayWidth){
      for( j <- 0 until ArrayHeight){
        dut.io.weight(j) #= weights(i)(j)
      }
      dut.clockDomain.waitSampling()
    }
    dut.io.weight.foreach(_ #= 0)
    dut.clockDomain.waitSampling()

    dut.io.load #= false
    dut.clockDomain.waitSampling()
    val macweights = dut.mac.map(_.map(_.weight.toInt).toArray).toArray /* mac weights */
    val reorder = reorderMatrix(macweights)
    val refweight = weights.flatten
    assert(reorder.sameElements(refweight),"load value error !!!")
  }

  test("vector * matrix"){
    /* */
    SIMCFG().compile{
     val dut = new InnerSystolicArray(SInt(16 bits),ArrayHeight,ArrayWidth)
      dut.mac.foreach(_.foreach(_.weight.simPublic())) /* for assert purpose */
      dut
    }.doSimUntilVoid{
      dut =>
        println(s"******* Array size : $ArrayHeight × $ArrayWidth *******")
        dut.clockDomain.forkStimulus(10)
        dut.io.load #= false
        dut.io.input.foreach(_ #= 0)
        dut.io.weight.foreach(_ #= 0)
        dut.clockDomain.waitSampling(2)

        val weights = Array.fill(ArrayWidth){Array.fill(ArrayHeight){Random.nextInt(100) - 50}}
        loadWeight(dut,weights = weights)

        val inputs = Array.fill(ArrayWidth){Random.nextInt(100) - 50}

        simSuccess()
    }

  }


  test("matrix * matrix"){

  }

}
