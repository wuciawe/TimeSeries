package regression

import org.apache.commons.math3.linear.{MatrixUtils, SingularValueDecomposition}

class RidgeRegression(x: IndexedSeq[IndexedSeq[Double]], y: IndexedSeq[Double]) {
  private val X = MatrixUtils.createRealMatrix(x.toArray.map(_.toArray))
  private val Y = y.toArray
  private var XsvdOpt: Option[SingularValueDecomposition] = None
  private var summaryOpt: Option[RidgeRegression.Summary] = None
  def summary = summaryOpt

  def update(l2penalty: Double = 0) = {
    val (mV, s, mU) = XsvdOpt match {
      case Some(xsvd) =>
        (xsvd.getV, xsvd.getSingularValues, xsvd.getU)
      case None =>
        val xsvd = new SingularValueDecomposition(X)
        XsvdOpt = Some(xsvd)
        (xsvd.getV, xsvd.getSingularValues, xsvd.getU)
    }

    val mS = MatrixUtils.createRealDiagonalMatrix(s.map(e => e / (math.pow(e, 2) + l2penalty)))
    val mZ = mV.multiply(mS).multiply(mU.transpose())

    val coefficients = mZ.operate(Y)
    val fitted = X.operate(coefficients)
    val residuals = new Array[Double](fitted.length)
    var errorVariance = 0.0
    residuals.indices.foreach{i =>
      val r = Y(i) - fitted(i)
      residuals(i) = r
      errorVariance += math.pow(r, 2)
    }
    errorVariance = errorVariance / (X.getRowDimension - X.getColumnDimension)

    val errorVarianceMatrix = MatrixUtils.createRealIdentityMatrix(Y.length).scalarMultiply(errorVariance)
    val coefficientsCovarianceMatrix = mZ.multiply(errorVarianceMatrix).multiply(mZ.transpose())
    val standarderrors = (0 until coefficientsCovarianceMatrix.getColumnDimension).map(i => coefficientsCovarianceMatrix.getEntry(i, i))

    summaryOpt = Some(new RidgeRegression.Summary(coefficients.toVector, standarderrors.toVector, l2penalty))
  }

}

object RidgeRegression {
  class Summary(val coefficients: Vector[Double], val standarderrors: Vector[Double], val l2penalty: Double)
}