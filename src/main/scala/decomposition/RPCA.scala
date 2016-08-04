package decomposition

import org.apache.commons.math3.linear.{MatrixUtils, SingularValueDecomposition}
import timeseries.util.MeanStddev

object RPCA {

  private val MAX_ITERS = 128

  def decompose(data: IndexedSeq[IndexedSeq[Double]], lpenalty: Double, spenalty: Double) = {
    val X = MatrixUtils.createRealMatrix(data.toArray.map(_.toArray))
    var L = MatrixUtils.createRealMatrix(X.getRowDimension, X.getColumnDimension)
    var S = MatrixUtils.createRealMatrix(X.getRowDimension, X.getColumnDimension)
    var E = MatrixUtils.createRealMatrix(X.getRowDimension, X.getColumnDimension)

    var mu = (4 * X.getData.foldLeft(0.0)((s, arr) => s + arr.foldLeft(0.0)((s, e) => s + math.abs(e)))) / (X.getColumnDimension * X.getRowDimension)
    var objPrev = 0.5 * Math.pow(X.getFrobeniusNorm, 2)
    var obj = objPrev
    val tol = 1e-8 * objPrev
    var diff = 2 * tol
    var iter = 0

    def computeL = {
      val LPenalty = lpenalty * mu
      val svd = new SingularValueDecomposition(X.subtract(S))
      val sv = svd.getSingularValues
      softThreshold(sv, LPenalty)
      val D_matrix = MatrixUtils.createRealDiagonalMatrix(sv)
      L = svd.getU.multiply(D_matrix).multiply(svd.getVT)
      sv.sum * LPenalty
    }

    def computeS = {
      val SPenalty = spenalty * mu
      val xl = X.subtract(L).getData
      softThreshold(xl, SPenalty)
      S = MatrixUtils.createRealMatrix(xl)
      xl.foldLeft(0.0)((s, arr) => s + arr.foldLeft(0.0)((s, e) => s + math.abs(e))) * SPenalty
    }

    def computeE = {
      E = X.subtract(L).subtract(S)
      Math.pow(E.getFrobeniusNorm, 2)
    }

    def computeDynamicMu = MeanStddev.stddev(E.getData.flatten) * math.sqrt(2 * math.max(E.getRowDimension, E.getColumnDimension))

    while(diff > tol && iter < MAX_ITERS) {
      val nuclearNorm = computeS
      val l1Norm = computeL
      val l2Norm = computeE

      obj = computeObjective(nuclearNorm, l1Norm, l2Norm)
      diff = Math.abs(objPrev - obj)
      objPrev = obj

      mu = computeDynamicMu

      iter = iter + 1
    }

    (L, S, E)
  }

  private def softThreshold(x: Array[Double], penalty: Double): Unit =
    x.indices.foreach{i => x(i) = math.signum(x(i)) * math.max(math.abs(x(i)) - penalty, 0)}

  private def softThreshold(x: Array[Array[Double]], penalty: Double): Unit =
    x.indices.foreach { i =>
      x(i).indices.foreach { j =>
        x(i)(j) = math.signum(x(i)(j)) * math.max(math.abs(x(i)(j)) - penalty, 0)
      }
    }

  private def computeObjective(nuclearnorm: Double, l1norm: Double, l2norm: Double) = 0.5 * l2norm + nuclearnorm + l1norm

}