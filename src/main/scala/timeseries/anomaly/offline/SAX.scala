package timeseries.anomaly.offline

import scala.collection.immutable.HashMap

/**
  * Created by jwhu on 7/30/16.
  */
object SAX {

  def genTrickingSignal(data: Vector[Double], precision: Short, window: Int, chunk: Int): Vector[Double] = {
    def generateSAX(): String = {
      val min = data.min
      val max = data.max
      val sh = (max - min) / precision
      val sections = new Array[Double](precision)

      (0 until precision).foreach { i =>
        sections(i) = min + i * sh
      }

      def generateSingle(d: Double): Char = {
        var i = 0
        var flag = true
        while(i < precision && flag) {
          if(sections(i) < d) flag = false
          if(flag) i += 1
        }
        i.toChar
      }

      data.map(generateSingle).mkString
    }

    val sax = generateSAX()

    def constructSAXMap(): (HashMap[Int, HashMap[String, Int]], HashMap[Int, HashMap[String, Int]]) = {

      def constructSAX(str: String): HashMap[String, Int] = {
        var frequency = HashMap.empty[String, Int]
        val len = str.length
        for { i <- 0 until len; if i + chunk <= len} {
          val c = str.substring(i, i + chunk)
          frequency = frequency + (c -> (frequency.getOrElse(c, 0) + 1))
        }
        frequency
      }

      var lagMaps = HashMap.empty[Int, HashMap[String, Int]]
      var futMaps = HashMap.empty[Int, HashMap[String, Int]]
      val cache = collection.mutable.HashMap.empty[String, HashMap[String, Int]]
      for { i <- sax.indices; if i >= window && i <= sax.length - window } {
        val lagMap = {
          val str = sax.substring(i - window, i)
          cache.get(str) match {
            case Some(m) => m
            case none => lagMaps.get(i - 1) match {
              case Some(last) =>
                val leaveC = sax.substring(i - window - 1, i - window + chunk - 1)
                val enterC = sax.substring(i - chunk, i)
                last + (leaveC -> (last(leaveC) - 1)) + (enterC -> (last.getOrElse(enterC, 0) + 1))
              case None => constructSAX(str)
            }
          }
        }
        val futMap = {
          val str = sax.substring(i, i + window)
          cache.get(str) match {
            case Some(m) => m
            case None => futMaps.get(i - 1) match {
              case Some(last) =>
                val leaveC = sax.substring(i - 1, i + chunk - 1)
                val enterC = sax.substring(i + window - chunk, i + window)
                last + (leaveC -> (last(leaveC) - 1)) + (enterC -> (last.getOrElse(enterC, 0) + 1))
              case None => constructSAX(str)
            }
          }
        }
        lagMaps = lagMaps + (i -> lagMap)
        futMaps = futMaps + (i -> futMap)
      }

      (lagMaps, futMaps)
    }

    val (lagMaps, futMaps) = constructSAXMap()

    def computeDiversity(index: Int): Double = {
      val lagMap = lagMaps(index)
      val futMap = futMaps(index)
      var d = 0.0
      val lKeys = lagMap.keySet
      val fKeys = futMap.keySet
      lKeys.intersect(fKeys).foreach{ k => d += math.pow(lagMap(k) - futMap(k), 2) }
      (lKeys -- fKeys).foreach{ k => d += math.pow(lagMap(k), 2) }
      (fKeys -- lKeys).foreach{ k => d += math.pow(futMap(k), 2) }
      d
    }

    data.indices.map(i => if(i < window || i > data.length - window) 0.0 else computeDiversity(i)).toVector
  }

}
