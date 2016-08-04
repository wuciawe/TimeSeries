package timeseries.correlation

import timeseries.util.AlignSeries

object PPMCC {
  private[this] type TS = (Long, Double)

  def correlate(s1: Vector[TS], s2: Vector[TS], maxStep: Int, impact: Double): Double = {
    val (ns1, ns2) = AlignSeries.align(s1.normalize(), s2.normalize())
    val m1 = ns1.mean()
    val std1 = ns1.stddev(m1)
    val m2 = ns2.mean()
    val std2 = ns2.stddev(m2)
    val denom = std1 * std2
    val nom = ns1.zip(ns2).foldLeft(0.0)((s, e) => s + (e._1._2 - m1) * (e._2._2 - m2))

    if(denom != 0) nom / denom else nom
  }
}
