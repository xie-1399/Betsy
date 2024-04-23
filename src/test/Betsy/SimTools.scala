package Betsy

import scala.collection.mutable.ArrayBuffer

object SimTools {

  /* with clip */
  def clipValue(width: Int, sign: Boolean, value: Int) = {
    val max = if (sign) Math.pow(2, width - 1) - 1 else Math.pow(2, width) - 1
    val min = if (sign) -Math.pow(2, width - 1) else 0
    val clip = if (value > max) {
      max.toInt
    }
    else if (value < min) {
      min.toInt
    }
    else {
      value
    }
    clip
  }

  def reorderMatrix(array: Array[Array[Int]]) = {
    val rows = array.length
    val cols = array(0).length
    val buffer = new ArrayBuffer[Int]()
    for (j <- cols - 1 to 0 by -1) {
      for (i <- 0 until rows) {
        buffer += array(i)(j)
      }
    }
    buffer.toArray
  }


}
