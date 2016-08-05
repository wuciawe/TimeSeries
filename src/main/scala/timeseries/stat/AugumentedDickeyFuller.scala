package timeseries.stat

import org.apache.commons.math3.linear.MatrixUtils
import timeseries.stat.Approx.RuleFV
//import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import regression.RidgeRegression
import timeseries.util.Difference

object AugumentedDickeyFuller {

  private[this] val table = Vector(
    Vector(-4.38, -3.95, -3.6, -3.24, -1.14, -.8, -.5, -.15),
    Vector(-4.15, -3.8, -3.5, -3.18, -1.19, -.87, -.58, -.24),
    Vector(-4.04, -3.73, -3.45, -3.15, -1.22, -.9, -.62, -.28),
    Vector(-3.99, -3.69, -3.43, -3.13, -1.23, -.92, -.64, -.31),
    Vector(-3.98, -3.68, -3.42, -3.13, -1.24, -.93, -.65, -.32),
    Vector(-3.96, -3.66, -3.41, -3.12, -1.25, -.94, -.66, -.33)
  )

  private[this] val tablen = table(0).length
  private[this] val tableT = Vector[Double](25, 50, 100, 250, 500, 1e5)
  private[this] val tablep = Vector(.01, .025, .5, .1, .9, .95, .975, .99)

  sealed trait Alternative
  case object Stationary extends Alternative
  case object Explosive extends Alternative

  def test(data: IndexedSeq[Double]): Double = test(data, Stationary)

  def test(data: IndexedSeq[Double], alternative: Alternative): Double = test(data, alternative, math.cbrt(data.length - 1).floor.toInt)

  def test(data: IndexedSeq[Double], alternative: Alternative, lag: Int): Double = {
    require(lag >= 0)

    val y = Difference.diff(data, 1)
    val k = lag + 1
    val n = y.length
    val z = MatrixUtils.createRealMatrix(embed(y, k)) //has rows length(ts) - 1 - k + 1
    val zcol1 = z.getColumnVector(0) //has length length(ts) - 1 - k + 1
    val xt1 = data.slice(k - 1, n)  //ts[k:(length(ts) - 1)], has length length(ts) - 1 - k + 1
    val trend = (k - 1) until n //trend k:n, has length length(ts) - 1 - k + 1
    val designMatrix =
      if(k > 1) MatrixUtils.createRealMatrix(data.length - 1 - k + 1, 3 + k - 1) // build design matrix as cbind(xt1, 1, trend, yt1)
      else MatrixUtils.createRealMatrix(data.length - 1 - k + 1, 3) // build design matrix as cbind(xt1, 1, tt)
    designMatrix.setColumn(0, xt1.toArray)
    designMatrix.setColumn(1, Array.fill[Double](data.length - 1 - k + 1)(1))
    designMatrix.setColumn(2, trend.toArray.map(_.toDouble))
    if (k > 1) {
      val yt1 = z.getSubMatrix(0, data.length - 1 - k, 1, k - 1) //same as z but skips first column
      designMatrix.setSubMatrix(yt1.getData, 0, 3)
    }

//    val regression = new OLSMultipleLinearRegression()
//    regression.setNoIntercept(true)
//    regression.newSampleData(zcol1.toArray, designMatrix.getData)
//    val beta = regression.estimateRegressionParameters()
//    val sd = regression.estimateRegressionParametersStandardErrors()

    val regression = new RidgeRegression(designMatrix.getData.map(_.toVector), zcol1.toArray)
    regression.update(.0001)
    val summary = regression.summary.get
    val t = summary.coefficients(0) / summary.standarderrors(0)

    val tableipl = (0 until tablen).map{ i => Approx.approx(tableT, Some(table.map(_(i))), Some(Vector(n)), rule = RuleFV)._2.head }
    val interpol = Approx.approx(tableipl, Some(tablep), Some(Vector(t)), rule = RuleFV)._2.head
    if(Approx.approx(tableipl, Some(tablep), Some(Vector(t)))._2.head.isNaN) {
      if(interpol == tablep.min) println(s"p-value smaller than printed p-value")
      else println(s"p-value greater than printed p-value")
    }

    alternative match {
      case Stationary => interpol
      case Explosive => 1 - interpol
    }

  }

  def isStationary(data: IndexedSeq[Double], p: Double = 0.05): Boolean = test(data) < p

  private[this] def embed(data: IndexedSeq[Double], dim: Int = 1): Array[Array[Double]] = {
    val res = Array.ofDim[Double](data.length - dim + 1, dim)
    (0 until dim).foreach{j =>
      res.indices.foreach{i =>
        res(i)(j) = data(dim - j - 1 + i)
      }
    }
    res
  }

}