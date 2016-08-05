package operations

object Convolution {

  sealed trait ConvMode
  case object Full extends ConvMode
  case object Valid extends ConvMode

  def convolve(x: IndexedSeq[Double], y: IndexedSeq[Double], mode: ConvMode): Vector[Double] = mode match {
    case Full => fullConvolve(x, y)
    case Valid => validConvolve(x, y)
  }

  private[this] def fullConvolve(x: IndexedSeq[Double], y: IndexedSeq[Double]): Vector[Double] = {
    val res = Array.fill(x.length + y.length - 1)(0.0)
    res.indices.foreach{ i =>
      val jmn = math.max(i - (y.length - 1), 0)
      val jmx = math.min(i, x.length - 1)
      (jmn to jmx).foreach{ j => res(i) += x(j) * y(i - j) }
    }
    res.toVector
  }

  private[this] def validConvolve(x: IndexedSeq[Double], y: IndexedSeq[Double]): Vector[Double] = {
    val (minV, maxV) = if(x.length < y.length) (x, y) else (y, x)
    val n = math.abs(x.length - y.length) + 1
    val res = Array.fill(n)(0.0)
    (0 until n).foreach{ i =>
      ((minV.length - 1) to 0 by - 1).zip(i until (i + minV.length)).foreach{ case (j, k) => res(i) += minV(j) * maxV(k) }
    }
    res.toVector
  }

}