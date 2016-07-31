package timeseries.correlation

import timeseries.util.AlignSeries

import scala.collection.mutable.ListBuffer

/**
  * Created by jwhu on 7/31/16.
  */
object CrossCorrelation {
  private[this] type TS = (Long, Double)

  def correlate(s1: Vector[TS], s2: Vector[TS], maxStep: Int, impact: Double): ((Int, Double), (Int, Double)) = {
    val (ns1, ns2) = AlignSeries.align(s1.normalize(), s2.normalize())
    val m1 = ns1.mean()
    val std1 = ns1.stddev(m1)
    val m2 = ns2.mean()
    val std2 = ns2.stddev(m2)
    val denom = std1 * std2 * ns1.length

    val lb = ListBuffer.empty[(Int, Double)]
    val slb = ListBuffer.empty[(Int, Double)]

    for(delay <- -maxStep to maxStep) {
      var s = 0.0
      ns1.indices.foreach{ i =>
        val j = i + delay
        if(j >= 0 && j < ns1.length) {
          s += (ns1(i)._2 - m1) * (ns2(j)._2 - m2)
        }
      }
      val r = if(denom != 0) s / denom else s
      lb.append((delay, r))
      val sr = r * (1 + delay.toDouble / maxStep * impact)
      slb.append((delay, sr))
    }

    (lb.maxBy(_._2), slb.maxBy(_._2))
  }
}
