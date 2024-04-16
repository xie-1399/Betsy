package Betsy.Experimental

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

  def printMatrix(matrix: Array[Array[Int]]): Unit = {
    for (row <- matrix) {
      for (elem <- row) {
        print(elem + " ")
      }
      println()
    }
    println()
  }
  
}