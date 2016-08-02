package timeseries.stat

import org.apache.commons.math3.linear.MatrixUtils
import regression.RidgeRegression
import timeseries.util.Difference

object ArgumentedDickeyFuller {

  private[this] val PVALUE_THRESHOLD = -3.45 //todo: should use a table to compute the p-value

  def test(data: Vector[Double]): ADFResult = test(data, math.cbrt(data.length - 1).floor.toInt)

  def test(data: Vector[Double], lag: Int): ADFResult = {
    val y = Difference.diff(data, 1)
    val k = lag + 1
    val n = data.length - 1
    val z = MatrixUtils.createRealMatrix(embed(y, k)) //has rows length(ts) - 1 - k + 1
    val zcol1 = z.getColumnVector(0) //has length length(ts) - 1 - k + 1
    val xt1 = data.slice(k - 1, n)  //ts[k:(length(ts) - 1)], has length length(ts) - 1 - k + 1
    val trend = 0 to (n - k) //trend k:n, has length length(ts) - 1 - k + 1
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

//    OLSMultipleLinearRegression regression = new OLSMultipleLinearRegression();
//    regression.setNoIntercept(true);
//    regression.newSampleData(zcol1.toArray(), designMatrix.getData());
//    double[] beta = regression.estimateRegressionParameters();
//    double[] sd = regression.estimateRegressionParametersStandardErrors();
    val regression = new RidgeRegression(designMatrix.getData.map(_.toVector), zcol1.toArray)
    regression.update(.0001)
    val summary = regression.summary.get
    val t = summary.coefficients(0) / summary.standarderrors(0)

    if (t <= PVALUE_THRESHOLD) {
      Stationary // reject the null hypothesis
    } else {
      NonStationary
    }
  }

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

sealed trait ADFResult
case object Stationary extends ADFResult
case object NonStationary extends ADFResult