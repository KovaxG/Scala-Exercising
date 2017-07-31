package Chapters

import Chapters.SimpleRNG._
import org.scalatest.{FreeSpec, Matchers}


class Chapter6Test extends FreeSpec with Matchers {
  // Exercise 6.1
  "looking at abs at Int.maxValue" in {
    SimpleRNG.positivize(Int.MaxValue) > 0 shouldBe true
  }

  "looking at abs at Int.MinValue" in {
    SimpleRNG.positivize(Int.MinValue) > 0 shouldBe true
  }

  "random test for random int" in {
    val rng = SimpleRNG(202)
    val (nr, seed2) = rng.nextInt
    nr > 0 shouldBe true
  }

  // Exercise 6.2
  "random test for random double between o and 1" in {
    val rng = SimpleRNG(202)
    val (dbnr, seed2) = rng.nextDouble
    ((0.0 <= dbnr) && (dbnr < 1.0)) shouldBe true
  }

  // Exercise 6.4
  "random list test" in {
    val list = SimpleRNG(202).ints(10)._1
    list.length shouldBe 10
  }

  // Exercise 6.6
  "map2 test" in {
    val rng = new SimpleRNG(1234)

    val ra: Rand[Int] = _int
    val rb: Rand[Double] = double

    val randIntDouble: Rand[(Int, Double)] = SimpleRNG.both(ra, rb)

    randIntDouble(rng) shouldBe randIntDouble(rng)
  }

  // Exercise 6.7
  "sequence test" in {
    val rng = new SimpleRNG(2011)

    val seq = sequence(List.fill(10)(_int))

    seq(rng) shouldBe seq(rng)
  }

  //Exercise 6.8
  "nonnegativelessthan imple,mentation" in {
    val rng = new SimpleRNG(1411)

    val nnlt: Rand[Int] = nonNegativeLessThan(23)

    nnlt(rng)._1 < 23 shouldBe true
  }
}
