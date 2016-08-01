package timeseries.anomaly.online

import timeseries.util.{ExpMovAvg, MeanStddev}

object Derivative {

  def genTrackingSignal(data: Vector[(Long, Double)], factor: Double): Vector[Double] = {
    val derivatives = new Array[Double](data.length)
    (1 until data.length).foreach{ i =>
      val dt = data(i)._1 - data(i - 1)._1
      val dv = data(i)._2 - data(i - 1)._2
      derivatives(i) = math.abs(if(dt != 0) dv / dt else dv)
    }
    derivatives(0) = derivatives(1)

    genSignal(data.map(_._2), ExpMovAvg.ema(derivatives, factor))
  }

  def genTrackingSignal(data: Vector[Double], factor: Double)(implicit d: DummyImplicit): Vector[Double] = {
    val derivatives = new Array[Double](data.length)
    (1 until data.length).foreach{ i => derivatives(i) = math.abs(data(i) - data(i - 1)) }
    derivatives(0) = derivatives(1)

    genSignal(data, ExpMovAvg.ema(derivatives, factor))
  }

  private[this] def genSignal(data: Vector[Double], derivative: Vector[Double]) = {
    val diff = new Array[Double](data.length)
    data.zipWithIndex.foreach{ case (v, i) => diff(i) = math.abs(v - derivative(i)) }
    val stddev = MeanStddev.stddev(diff)
    if(stddev != 0.0) diff.map(_ / stddev).toVector else diff.toVector
  }

}
