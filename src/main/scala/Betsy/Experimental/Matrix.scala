package Betsy.Experimental

import scala.collection.mutable.ArrayBuffer

object Matrix {

  /* GEMM Matrix Mul */
  def multiply(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Array[Array[Int]] = {
    val rows1 = matrix1.length
    val cols1 = matrix1(0).length
    val cols2 = matrix2(0).length
    val result = Array.ofDim[Int](rows1, cols2)

    for (i <- 0 until rows1) {
      for (j <- 0 until cols2) {
        var sum = 0
        for (k <- 0 until cols1) {
          sum += matrix1(i)(k) * matrix2(k)(j)
        }
        result(i)(j) = sum
      }
    }
    result
  }

  /* the conv calculate using scala */
  def convCalculate(filter: Array[Array[BigInt]], featuremap: Array[Array[BigInt]], logout: Boolean = true) = {
    val filterHeight = filter.length
    val filterWidth = filter(0).length
    val inputHeight = featuremap.length
    val inputWidth = featuremap(0).length

    val result = Array.ofDim[BigInt](inputHeight - filterHeight + 1, inputWidth - filterWidth + 1)

    for (i <- 0 until inputHeight - filterHeight + 1) {
      for (j <- 0 until inputWidth - filterWidth + 1) {
        var sum = BigInt(0)
        for (x <- 0 until filterHeight) {
          for (y <- 0 until filterWidth) {
            sum += filter(x)(y) * featuremap(i + x)(j + y)
          }
        }
        result(i)(j) = sum
      }
    }
    if (logout) {
      println("Convolution Result:")
      for (row <- result) {
        println(row.mkString(" "))
      }
    }
    result
  }

  def printMatrix(matrix: Array[Array[Int]]): Unit = {
    for (row <- matrix) {
      for (elem <- row) {
        print(elem + " ")
      }
      println()
    }
    println()
  }

  /* also fill with zero */
  def mkArray(size: Int, cycles: Int) = {
    val array = Array.fill(cycles)(0)
    var arrayIndex = 0
    val remain = cycles - (2 * size - 1)
    require(remain >= 0)
    for (idx <- 1 to size) {
      array(arrayIndex) = idx
      arrayIndex += 1
    }
    for (idx <- size - 1 until 0 by -1) {
      array(arrayIndex) = idx
      arrayIndex += 1
    }
    for (idx <- 0 until remain) {
      array(arrayIndex) = 0
      arrayIndex += 1
    }
    array
  }

  /* reshape the data format to the Array */
  def TPUreshape(raw: Array[Int], size: Int, cycles: Int) = {
    require(raw.length == size * size)
    require(cycles >= size * 2 + 1)
    val bufferArray = new ArrayBuffer[Array[Int]]()
    var continue = 0
    val numArray = mkArray(size, cycles)

    for (idx <- 0 until cycles) {
      val array = Array.fill(size) {
        0
      }
      if (continue == raw.length) {
        bufferArray += array
      } else {
        val num = numArray(idx)
        if (idx < size) {
          for (index <- 0 until num) {
            array(index) = raw(continue + index)
          }
        } else {
          for (index <- 0 until num) {
            array(size + index - num) = raw(continue + index)
          }
        }
        continue += num
        bufferArray += array
      }
    }
    bufferArray
  }

  def orderchange(arr: Array[Int]): Array[Int] = {
    val n = arr.length
    val middleStart = (n - 1) / 2
    val middleEnd = (n + 2) / 2
    if (n >= 4) {
      val temp = arr(middleStart)
      arr(middleStart) = arr(middleStart + 1)
      arr(middleStart + 1) = temp
    }
    for (i <- middleStart + 1 until middleEnd) {
      val temp = arr(i)
      arr(i) = arr(n - i - 1)
      arr(n - i - 1) = temp
    }
    if (n >= 8) {
      val temp2 = arr(n - 2)
      arr(n - 2) = arr(n - 3)
      arr(n - 3) = temp2
    }
    arr
  }

  def diagArrange(matrix: Array[Array[Int]],up:Boolean = true): Array[Int] = {
    val numRows = matrix.length
    val numCols = matrix(0).length

    var rearrangedElements = Array[Int]()
      for (sum <- 0 until numRows + numCols - 1) {
        val startRow = if (sum < numRows) 0 else sum - numRows + 1
        val endRow = if (sum < numCols) sum else numCols - 1
        for (i <- startRow to endRow) {
          val j = sum - i
          rearrangedElements :+= matrix(i)(j)
        }
    }
    rearrangedElements
  }
}
