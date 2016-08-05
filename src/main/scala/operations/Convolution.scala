package operations

object Convolution {

  sealed trait ConvMode
  case object Full extends ConvMode
  case object Valid extends ConvMode
  case object Same extends ConvMode

  def convolve(x: IndexedSeq[Double], y: IndexedSeq[Double], mode: ConvMode): Vector[Double] = {
    val (shorter, longer) = if(x.length < y.length) (x, y) else (y, x)
    val (res, s) = mode match{
      case Full => (Array.fill(longer.length + shorter.length - 1)(0.0), 0)
      case Same => (Array.fill(longer.length)(0.0), (shorter.length - 1) / 2)
      case Valid => (Array.fill(longer.length - shorter.length + 1)(0.0), shorter.length - 1)
    }
    res.indices.foreach{ n =>
      (math.max(n + s - (shorter.length - 1), 0) to math.min(n + s, longer.length - 1)).foreach{ m =>
        res(n) += longer(m) * shorter(n + s - m)
      }
    }
    res.toVector
  }

}