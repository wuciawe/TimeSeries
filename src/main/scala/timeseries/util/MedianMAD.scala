package timeseries.util

import scala.annotation.tailrec

object MedianMAD {

  private[this] case class ArrayView(arr: Array[Double], from: Int, until: Int) {
    def apply(n: Int) =
      if (from + n < until) arr(from + n)
      else throw new ArrayIndexOutOfBoundsException(n)

    def partitionInPlace(p: Double => Boolean): (ArrayView, ArrayView) = {
      var upper = until - 1
      var lower = from
      while (lower < upper) {
        while (lower < until && p(arr(lower))) lower += 1
        while (upper >= from && !p(arr(upper))) upper -= 1
        if (lower < upper) { val tmp = arr(lower); arr(lower) = arr(upper); arr(upper) = tmp }
      }
      (copy(until = lower), copy(from = lower))
    }

    def size = until - from
    def isEmpty = size <= 0
  }

  private[this] object ArrayView {
    def apply(arr: Array[Double]) = new ArrayView(arr, 0, arr.length)
  }

  @tailrec
  private[this] def findKInPlace(arr: ArrayView, k: Int)(implicit choosePivot: ArrayView => Double): Double = {
    val a = choosePivot(arr)
    val (s, b) = arr partitionInPlace (a > _)
    if (s.size == k) a
    // The following test is used to avoid infinite repetition
    else if (s.isEmpty) {
      val (s, b) = arr partitionInPlace (a == _)
      if (s.size > k) a
      else findKInPlace(b, k - s.size)
    } else if (s.size < k) findKInPlace(b, k - s.size)
    else findKInPlace(s, k)
  }

  private[this] def medianInPlace(arr: Array[Double])(implicit choosePivot: ArrayView => Double): Double =
    if(arr.length % 2 == 0) (findKInPlace(ArrayView(arr), (arr.length - 1) / 2) + findKInPlace(ArrayView(arr), arr.length / 2)) / 2
    else findKInPlace(ArrayView(arr), (arr.length - 1) / 2)

  def median(data: Seq[Double]): Double = {
    val mData = Array.ofDim[Double](data.length)
    data.copyToArray(mData)
    medianInPlace(mData)(arr => arr(scala.util.Random.nextInt(arr.size)))
  }

  def mad(data: Seq[Double], median: Double): Double =  this.median(data.map(e => math.abs(e - median)))

  def mad(data: Seq[Double]): Double = mad(data, median(data))

  def msigma(data: Seq[Double], median: Double): Double = 1.4826 * mad(data, median)

  def msigma(data: Seq[Double]): Double = msigma(data, median(data))

}
