package timeseries.anomaly.online

object AbsoluteThreshold {

  def genTrackingSignal(data: Vector[Double], threshold: Either[Double, Double]): Vector[Double] = {
    val mapper: Double => Double = threshold match {
      case Left(l) => (v: Double) => if(v < l) l - v else 0.0
      case Right(u) => (v: Double) => if(v > u) v - u else 0.0
    }

    data map mapper
  }

}
