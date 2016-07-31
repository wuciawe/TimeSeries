package timeseries.util

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by jwhu on 7/31/16.
  */
object AlignSeries {
  private[this] type TS = (Long, Double)

  def align(s1: Vector[TS], s2: Vector[TS]): (Vector[TS], Vector[TS]) = {
    val lb1 = ListBuffer.empty[TS]
    val lb2 = ListBuffer.empty[TS]

    val iter1 = s1.toIterator
    val iter2 = s2.toIterator

    @tailrec
    def ga(i1: TS, i2: TS): Unit = {
      if(i1._1 == i2._1) {
        lb1.append(i1)
        lb2.append(i2)
        if(iter1.hasNext && iter2.hasNext) ga(iter1.next(), iter2.next())
      } else if(i1._1 < i2._1) {
        lb1.append(i1)
        lb2.append((i1._1, i2._2))
        if(iter1.hasNext && iter2.hasNext) ga(iter1.next(), i2)
      } else {
        lb1.append((i2._1, i1._2))
        lb2.append(i2)
        if(iter1.hasNext && iter2.hasNext) ga(i1, iter2.next())
      }
    }

    if(iter1.hasNext && iter2.hasNext) ga(iter1.next(), iter2.next())

    def gr(iter: Iterator[TS], ta: ListBuffer[TS], ca: ListBuffer[TS]): Unit = {
      @tailrec
      def rgr(c: Double): Unit = {
        if(iter.hasNext) {
          val (t, v) = iter.next()
          ta.append((t, v))
          ca.append((t, c))
          rgr(c)
        }
      }
      rgr(ca.lastOption.fold(0.0)(_._2))
    }

    gr(iter1, lb1, lb2)
    gr(iter2, lb2, lb1)

    (lb1.toVector, lb2.toVector)
  }
}
