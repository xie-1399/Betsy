package Betsy

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


}
