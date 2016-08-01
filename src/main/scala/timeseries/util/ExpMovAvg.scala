package timeseries.util

object ExpMovAvg {

  def ema(data: IndexedSeq[Double], factor: Double): Vector[Double] =
    ema(data, 0, data.length, factor)

  def ema(data: IndexedSeq[Double], start: Int, end: Int, factor: Double): Vector[Double] = {
    require(start >= 0 && start < end && end <= data.length)

    val res = new Array[Double](end - start)
    res(0) = data(start)
    (1 until res.length).foreach { i => res(i) = factor * data(start + i) + (1 - factor) * res(i - 1)}
    res.toVector
  }
}
