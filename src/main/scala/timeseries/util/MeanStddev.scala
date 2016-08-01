package timeseries.util

object MeanStddev {

  def mean(data: Seq[Double]): Double = if(data.nonEmpty) data.sum / data.length else Double.NaN

  def stddev(data: Seq[Double], mean: Double): Double =
    if(data.nonEmpty) math.sqrt(data.foldLeft(0.0)((s, d) => s + math.pow(d - mean, 2)) / data.length)
    else Double.NaN

  def stddev(data: Seq[Double]): Double = stddev(data, mean(data))
}
