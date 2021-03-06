package decomposition

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix, SingularValueDecomposition}

object RPCAX {

  private val MAX_ITERS = 128

  def decompose(data: IndexedSeq[IndexedSeq[Double]], lambdaCoef: Double = 2.368) = {
    val M = MatrixUtils.createRealMatrix(data.toArray.map(_.toArray))
    var L = MatrixUtils.createRealMatrix(M.getRowDimension, M.getColumnDimension)
    var S = MatrixUtils.createRealMatrix(M.getRowDimension, M.getColumnDimension)
    var Y = MatrixUtils.createRealMatrix(M.getRowDimension, M.getColumnDimension)

    val mu = M.getColumnDimension * M.getRowDimension / (4 * M.getData.foldLeft(0.0)((s, arr) => s + arr.foldLeft(0.0)((s, e) => s + math.abs(e))))
    val lambda = lambdaCoef / math.sqrt(math.max(M.getColumnDimension, M.getRowDimension))
    val tol = 1e-7 * M.getFrobeniusNorm
    var diff = 2 * tol
    var iter = 0

    def updateL(muiY: RealMatrix) = {
      val svd = new SingularValueDecomposition(M.subtract(S).add(muiY))
      val sv = svd.getSingularValues
      softThreshold(sv, 1 / mu)
      val D_matrix = MatrixUtils.createRealDiagonalMatrix(sv)
      L = svd.getU.multiply(D_matrix).multiply(svd.getVT)
    }

    def updateS(muiY: RealMatrix) = {
      val xl = M.subtract(L).add(muiY).getData
      softThreshold(xl, lambda / mu)
      S = MatrixUtils.createRealMatrix(xl)
    }

    def updateY() = {
      Y = Y.add(M.subtract(L).subtract(S).scalarMultiply(mu))
    }

    while(diff > tol && iter < MAX_ITERS) {
      val muiY = Y.scalarMultiply(1 / mu)
      updateL(muiY)
      updateS(muiY)
      updateY()

      diff = M.subtract(L).subtract(S).getFrobeniusNorm

      iter = iter + 1
    }

    (L, S, Y)
  }

  private def softThreshold(x: Array[Double], penalty: Double): Unit =
    x.indices.foreach{i => x(i) = math.signum(x(i)) * math.max(math.abs(x(i)) - penalty, 0)}

  private def softThreshold(x: Array[Array[Double]], penalty: Double): Unit =
    x.indices.foreach { i =>
      x(i).indices.foreach { j =>
        x(i)(j) = math.signum(x(i)(j)) * math.max(math.abs(x(i)(j)) - penalty, 0)
      }
    }

}