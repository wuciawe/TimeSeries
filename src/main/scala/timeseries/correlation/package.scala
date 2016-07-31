package timeseries

/**
  * Created by jwhu on 7/31/16.
  */
package object correlation {

  implicit class TSImp(ts: Vector[(Long, Double)]) {

    def normalize(): Vector[(Long, Double)] = {
      val r = new Array[(Long, Double)](ts.length)
      val max = {
        val m = math.abs(ts.maxBy(e => math.abs(e._2))._2)
        if(m == 0) 1 else m
      }
      ts.zipWithIndex.foreach{ case ((t, v), i) => r(i) = (t, v / max)}
      r.toVector
    }

    def mean(): Double = ts.foldLeft(0.0)((s, e) => s + e._2) / ts.length

    def stddev(mean: Double) = math.sqrt(ts.foldLeft(0.0)((s, e) => s + math.pow(e._2 - mean, 2)) / ts.length)
  }
}
