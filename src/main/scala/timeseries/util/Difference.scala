package timeseries.util

object Difference {

  def diff(data: IndexedSeq[Double], order: Int): Vector[Double] = {
    val res = new Array[Double](data.length - order)
    res.indices.foreach{i => res(i) = data(i + order) - data(i)}
    return res.toVector
  }
}
