package Betsy.Experimental

/**
 ** Sloan follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/15      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the Eyeriss array using the RS dataflow **
 ** pass the weight horizontal / pass the psum row by row / pass the ifmap in the diag **
 ** the Eyeriss PE calculate the vec mul in one cycle **
 */


import spinal.core._
import spinal.lib._
import Betsy.Until.BetsyModule

/*
 * PE contains calculate the MAC between filter and weight
 * this shows very simple way to calculate the MAC with PE Array
 * calculate with the row by row
 * the dataflow is RS and the eyeriss is also RS like
*/
class EyerissPEV1(filterLen:Int,fmapLen:Int,dataWidth:Int) extends BetsyModule {
  require(filterLen <= fmapLen)
  val io = new Bundle{
    val filterRow = in(Vec(SInt(dataWidth bits),filterLen))
    val fmapRow = in(Vec(SInt(dataWidth bits),fmapLen))
    val sumIn = in(Vec(SInt(2 * dataWidth bits),fmapLen - filterLen + 1))
    val filterRowOut = out(Vec(SInt(dataWidth bits),filterLen))
    val sumOut = out(Vec(SInt(2 * dataWidth bits),fmapLen - filterLen + 1))
  }

  val localConvSum = Vec(Vec(SInt( 2 * dataWidth bits),filterLen),fmapLen - filterLen + 1)
  val tempSum = Vec(SInt(2 * dataWidth bits),fmapLen - filterLen + 1)

  /* simple set the stride is 1 */
  for(row <- 0 until fmapLen - filterLen + 1){
    for(col <- 0 until filterLen){
      localConvSum(row)(col) := io.filterRow(col) * io.fmapRow(col + row)
    }
    tempSum(row) := localConvSum(row).reduceBalancedTree(_ + _)
  }
  for(idx <- 0 until fmapLen - filterLen + 1){
    io.sumOut(idx) := tempSum(idx) + io.sumIn(idx)
  }

  io.filterRowOut := io.filterRow
}

/* set PE Array Here and can be set with pipe line
* because each cycle can calculate one line -> the cycle is enough can be out*/

class EyerissPEArrayV1 (filterRowNum:Int,fmapRowNum:Int,
                     filterLen:Int,fmapLen:Int,dataWidth:Int,passFmap:Boolean = true) extends BetsyModule {
  require(fmapRowNum >= filterRowNum)
  val io = new Bundle{
    val filterIn = in Vec(Vec(SInt(dataWidth bits),filterLen),filterRowNum)
    val fmapIn = in Vec(Vec(SInt(dataWidth bits),fmapLen),fmapRowNum)
    val sum = out(Vec(Vec(SInt(2 * dataWidth bits),fmapLen - filterLen + 1),fmapRowNum - filterRowNum + 1))
  }

  val PEs = Array.fill(filterRowNum,fmapRowNum - filterRowNum + 1){   /* create the PE Array using this way */
    new EyerissPEV1(filterLen, fmapLen, dataWidth)
  }

  /* pass the filter and fmap in */
  for(idx <- 0 until filterRowNum){
    PEs(idx)(0).io.filterRow := io.filterIn(idx)
  }

  /* connect the psum */
  PEs(0).map(_.io.sumIn.map(_ := 0))
  for(row <- 1 until filterRowNum){
    for(col <- 0 until fmapRowNum - filterRowNum + 1){
      PEs(row)(col).io.sumIn := PEs(row - 1)(col).io.sumOut
    }
  }
  (io.sum,PEs(filterRowNum -1)).zipped.map(_ := _.io.sumOut)

  /* the filter will pass from left to right and let the feature map in*/
  for(row <- 0 until filterRowNum){
    for(col <- 1 until fmapRowNum - filterRowNum + 1){
      PEs(row)(col).io.filterRow := PEs(row)(col - 1).io.filterRowOut
    }
  }
  /* let the feature map in */
  if(passFmap){
    for (idx <- 0 until fmapRowNum - filterRowNum + 1) {
      PEs(0)(idx).io.fmapRow := io.fmapIn(idx)
    }
    for (idx <- 1 until filterRowNum) {
      PEs(idx)(fmapRowNum - filterRowNum).io.fmapRow := io.fmapIn(filterRowNum + idx - 1)
    }
    for (row <- 1 until filterRowNum) {
      for (col <- 0 until fmapRowNum - filterRowNum) {
        PEs(row)(fmapRowNum - filterRowNum - 1 - col).io.fmapRow := PEs(row - 1)(fmapRowNum - filterRowNum - col).io.fmapRow
      }
    }
  }
  else{
    for (row <- 0 until filterRowNum) {
      for (col <- 0 until fmapRowNum - filterRowNum + 1) {
        (PEs(row)(col).io.fmapRow, io.fmapIn(row + col)).zipped.map(_ := _)
      }
    }
  }
}
