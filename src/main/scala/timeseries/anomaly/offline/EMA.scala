package timeseries.anomaly.offline

import timeseries.util.{ExpMovAvg, MeanStddev}

object EMA {

  def genTrackingSignal(data: Vector[Double], factor: Double, window: Option[Int] = None): Vector[Double] = window match {
    case Some(w) =>
      val stddev = MeanStddev.stddev(data)
      if(stddev != 0.0)
        data.zipWithIndex.map{ case(d, i) => math.abs(d - ExpMovAvg.ema(data, math.max(0, i - w), i + 1, factor).last) / stddev }
      else
        data.zipWithIndex.map{ case(d, i) => math.abs(d - ExpMovAvg.ema(data, math.max(0, i - w), i + 1, factor).last) }
    case None =>
      val ema = ExpMovAvg.ema(data, factor)
      val stddev = MeanStddev.stddev(ema)
      if(stddev != 0.0) data zip ema map {case(d, e) => math.abs(d - e) / stddev}
      else data zip ema map {case(d, e) => math.abs(d - e)}
  }

}
