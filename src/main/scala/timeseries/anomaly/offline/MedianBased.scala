package timeseries.anomaly.offline

import timeseries.util.MedianMAD

object MedianBased {

  def genTrackingSignal(data: Vector[Double], period: Int, numPeriod: Int): Vector[Double] = {
    val res = Array.fill(period)(0.0)
    val d = Array.fill(numPeriod)(0.0)
    res.indices.foreach{ i =>
      d.indices.foreach{ di =>
        d(di) = data(di * period + i)
      }
      val median = MedianMAD.median(d)
      val mad = MedianMAD.mad(d, median)
      res(i) = math.abs(d(numPeriod * period + i) - median) / (if(mad != 0.0) mad else 1)
    }
    res.toVector
  }

}
