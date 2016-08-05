package timeseries.anomaly.offline

import operations.Convolution
import org.apache.commons.math3.distribution.BinomialDistribution

/**
  * Created by jwhu on 8/5/16.
  */
object SignTest {

  def genTrackingSignal(data: Vector[Double], baseline: Vector[Double], percent: Double, window: Int, offset: Double = 0.0, conf: Double = 0.01): Vector[Double] = {
    val scale = if(percent > 0) 1 else -1
    val anoms = rolling(data.map(scale * _), baseline.map(scale * _), window, conf, math.abs(percent) / 100, scale * offset)
    val scores = Array.fill(data.length)(0.0)
    anoms.foreach{ case ((s, e), p) => (s until e).foreach{scores(_) = p} }
    scores.toVector
  }

  def mergeRanges(ranges: Seq[(Int, Int)], maxGap: Double): Vector[(Int, Int)] = {
    val mranges = collection.mutable.ListBuffer.empty[(Int, Int)]
    var lastRange = ranges.head
    ranges.tail.foreach{
      case (cStart, cEnd) =>
        if(cStart - lastRange._2 < maxGap){
          // merge current range with the last range in the list
            lastRange = (lastRange._1, math.max(cEnd, lastRange._2))
        } else {
          // append the new range to current one
          mranges.append(lastRange)
          lastRange = (cStart, cEnd)
        }
    }
    mranges.append(lastRange)
    mranges.toVector
  }

  def rolling(x: IndexedSeq[Double], y: IndexedSeq[Double], k: Int = 24, alpha: Double = 0.05, offset: Double = 0.0, conf: Double = 0.01, gap: Int = 0) = {

    // filter to convolve with - just counts
    val filter = Array.fill(k)(1.0)

    // threshold to be bigger than
    val qThreshold = new BinomialDistribution(k, 0.5).inverseCumulativeProbability(1 - conf) - 1

    // this is 1 if bigger 0 otherwise
    val d = x.zip(y).map{ case (e1, e2) => math.max(math.signum(e1 - offset - (1 + alpha) * e2), 0) }

    val con = Convolution.convolve(filter, d, Convolution.Valid)

    val a = con.map(e => math.max(e - qThreshold, 0))

    val ranges = mergeRanges(a.zipWithIndex.filter(_._1 != 0).map(e => (e._2, e._2 + k)), gap)

    // compute confidence
    val probs = collection.mutable.ListBuffer.empty[Double]
    ranges.foreach{ case(s, e) => probs.append(new BinomialDistribution(d.slice(s, e).sum.toInt, 0.5).cumulativeProbability(e - s)) }
    ranges.zip(probs)
  }

}
