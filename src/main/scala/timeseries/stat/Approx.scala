package timeseries.stat

import scala.collection.mutable.ListBuffer

/**
  * Created by jwhu on 8/4/16.
  */
object Approx {

  sealed trait Ties
  case object Mean extends Ties
  case object Ordered extends Ties
  case object NoTies extends Ties

  sealed trait Method
  case object Linear extends Method
  case object Constant extends Method

  sealed trait RuleT
  case object RuleNaN extends RuleT
  case object RuleFV extends RuleT

  case class Rule(rl: RuleT, rr: RuleT)
  object Rule{
    import scala.language.implicitConversions
    implicit def cvt(r: RuleT): Rule = Rule(r, r)
  }

  def approx(x: IndexedSeq[Double], yOpt: Option[IndexedSeq[Double]], xoutOpt: Option[IndexedSeq[Double]] = None,
             n: Int = 50, method: Method = Linear,
             yLeftOpt: Option[Double] = None, yRightOpt: Option[Double] = None,
             rule: Rule = RuleNaN, f: Double = 0, ties: Ties = Mean): (Vector[Double], Vector[Double]) = {
    val (nx, ny) = regularize(x, yOpt, ties)
    val lenNX = nx.length
    if(lenNX == 0) throw new RuntimeException("zero non-NA points")
    if(lenNX == 1 && method == Linear) throw new RuntimeException("need at least two non-NA values to interpolate")
    val yLeft = yLeftOpt match {
      case Some(yl) => yl
      case None => rule.rl match {
        case RuleNaN => Double.NaN
        case RuleFV => ny.head
      }
    }
    val yRight = yRightOpt match {
      case Some(yr) => yr
      case None => rule.rr match {
        case RuleNaN => Double.NaN
        case RuleFV => ny.last
      }
    }
    val xout = xoutOpt match {
      case Some(xo) => xo.toVector
      case None =>
        if(n <= 0) throw new RuntimeException("approx require n >= 1")
        genSeq(nx.head, nx.last, n)
    }
    (xout, approx(nx, ny, xout, method, yLeft, yRight, f))
  }

  private[this] def regularize(x: IndexedSeq[Double], yOpt: Option[IndexedSeq[Double]], ties: Ties): (Vector[Double], Vector[Double]) = {
    val y = yOpt.getOrElse(x)
    ties match {
      case Ordered => (x.toVector, y.toVector)
      case Mean =>
        val (sx, sy) = (x zip y).sortBy(_._1).unzip
        val ux = sx.distinct
        if(ux.length < x.length){
          val gIndexes = matchIndex(x, x)
          val r = Array.fill(gIndexes.max + 1)(ListBuffer.empty[Double])
          gIndexes.zip(y).foreach{ case (i, e) => r(i).append(e) }
          (ux.toVector, r.map(l => l.sum / l.length).toVector)
        } else (sx.toVector, sy.toVector)
      case NoTies =>
        val (sx, sy) = (x zip y).sortBy(_._1).unzip
        val ux = sx.distinct
        if(ux.length < x.length){
          val gIndexes = matchIndex(x, x)
          println("warning collapsing to unique 'x' values") // todo: use log
          (ux.toVector, gIndexes.distinct.map(_.toDouble))
        } else (sx.toVector, sy.toVector)
    }
  }

  private[this] def matchIndex[A](x: Seq[A], table: IndexedSeq[A], noMatch: Int = -1): Vector[Int] = {
    val res = Array.ofDim[Int](x.length)
    x.zipWithIndex.foreach{
      case (e, i) =>
        val ind = table.indexOf(e)
        res(i) = if(ind >= 0) ind else noMatch
    }
    res.toVector
  }

  private[this] def genSeq(from: Double, to: Double, len: Int): Vector[Double] = {
    val by = (to - from) / (len - 1)
    (0 until len).map(i => from + i * by).toVector
  }

  private[this] def approxTest(x: IndexedSeq[Double], y: IndexedSeq[Double], method: Method, sf: Double): Unit = {
    method match {
      case Linear =>
      case Constant => if(sf < 0 || sf > 1) throw new IllegalArgumentException("approx(): invalid f value")
    }
    x.indices.foreach{i => if(x(i).isNaN || y(i).isNaN) throw new IllegalArgumentException("approx(): attempted to interpolate NA values")}
  }

  private[this] def approx(x: IndexedSeq[Double], y: IndexedSeq[Double], xout: IndexedSeq[Double], method: Method, yLeft: Double, yRight: Double, sf: Double): Vector[Double] = {
    val f2 = sf
    val f1 = 1 - sf

    def approx1(v: Double): Double = {
      if(x.isEmpty) Double.NaN
      else {
        /* Approximate  y(v),  given (x,y)[i], i = 0,..,n-1 */
        var i = 0
        var j = x.length - 1

        /* handle out-of-domain points */
        if(v < x(i)) yLeft
        else if(v > x(j)) yRight
        else {
          /* find the correct interval by bisection */
          var ij = 0
          while(i < j - 1) { /* x[i] <= v <= x[j] */
            ij = (i + j) / 2; /* i+1 <= ij <= j-1 */
            if(v < x(ij)) j = ij else i = ij
            /* still i < j */
          }
          /* provably have i == j-1 */

          /* interpolation */
          if(v == x(j)) y(j)
          else if(v == x(i)) y(i)
          /* impossible: if(x[j] == x[i]) return y[i]; */
          else method match {
            case Linear => y(i) + (y(j) - y(i)) * ((v - x(i)) / (x(j) - x(i))) /* linear */
            case Constant => y(i) * f1 + y(j) * f2 /* constant */
          }
        }
      }
    }

    xout.map(e => if(e.isNaN) e else approx1(e)).toVector
  }

}
