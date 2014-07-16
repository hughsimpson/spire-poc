import spire.algebra.MetricSpace
import spire.algebra.MetricSpace._
import spire.algebra.IsReal._

import spire.implicits.{IntAlgebra}
import spire.math.{min => _,_}
import scala.math.min

import scala.util.Random


object Main extends App {
  implicit object Levenshtein extends MetricSpace[String,Int] {
    /// originally 10e5=5753849258,5519761678,4242966259,4950484545  10e6 = 48638904526

    def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)

    def distance(s1: String, s2: String): Int = {
      val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0}

      for (j <- 1 to s2.length; i <- 1 to s1.length)
        dist(j)(i) = if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
        else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)
      if (Random.nextInt(1000000) % 123456 == 0) println("tooth")
      dist(s2.length)(s1.length)
    }
  }
  val repetitions = 10e5.toLong

    val start = System.nanoTime()
    (0L until repetitions).map(_ => closeTo("kitten", "sitting", 3d))
    (0L until repetitions).map(_ => closeTo("rosettacode", "raisethysword", 10))
    println(System.nanoTime() - start)

}