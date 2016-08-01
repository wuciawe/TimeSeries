package timeseries.anomaly.online

object DiffPercentage {

  def genTrackingSignal(data: Vector[Double], base: Vector[Double]): Vector[Double] = {
    require(data.length == base.length)
    data zip base map {
      case (d, b) if b != 0.0 => (d - b) / b
      case (d, b) if d != 0.0 => 1.0
      case _ => 0.0
    }
  }

}
