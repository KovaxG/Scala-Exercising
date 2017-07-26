package Chapters

import org.scalatest.{FreeSpec, Matchers}
import Streaming.Stream

class Chapter5Test extends FreeSpec with  Matchers{
  // Exercise 5.1: Stream to List
  "Stream to list" in {
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

  // Exercise 5.2
  "taking test" in {
    Stream(1, 2, 3, 4, 5).take(3).toList shouldBe Stream(1, 2, 3).toList
  }

  "drop zero stuff" in {
    Stream(1, 2, 3, 4, 5).drop(0).toList shouldBe Stream(1, 2, 3, 4, 5).toList
  }

  "drop 1 stuff" in {
    Stream(1, 2, 3, 4, 5).drop(1).toList shouldBe Stream(2,3,4,5).toList
  }

  "dropping test" in {
    Stream(1, 2, 3, 4, 5).drop(3).toList shouldBe Stream(4, 5).toList
  }

  // Exercise 5.3
  "takeWhile test" in {
    Stream(1, 2, 3, 4, 5).takeWhile((p: Int) => p <= 3).toList shouldBe Stream(1, 2, 3).toList
  }

  // Exercise 5.4
  "for all test success" in {
    Stream(1, 2, 3, 4, 5).forAll((p: Int) => p < 10) shouldBe true
  }

  "for all test fail" in {
    Stream(1, 2, 3, 4, 5).forAll((p: Int) => p > 3) shouldBe false
  }

  // Exercise 5.5
  "takeWhile foldright" in {
    Stream(1, 2, 3, 4, 5).takeWhile2((p: Int) => p <= 3).toList shouldBe Stream(1, 2, 3).toList
  }

  // Exercise 5.6 headOption
  "head of empty" in {
    Stream().headOption shouldBe None
  }

  "head of nominal" in {
    Stream(1, 2, 3, 4).headOption shouldBe Some(1)
  }

  // Exercise 5.7
  "lazy map" in {
    Stream(1, 2, 3, 4, 5).map(a => a + 1).toList shouldBe Stream(2, 3, 4, 5, 6).toList
  }

  "lazy filter" in {
    Stream(1, 2, 3, 4, 5).filter((a: Int) => a < 3).toList shouldBe Stream(1, 2).toList
  }

  // Ones
  "take 3 ones" in {
    Stream.ones.take(3).toList shouldBe Stream(1, 1, 1).toList
  }

  // Exercise 5.8
  "take 3 \"Cool\" " in {
    Stream.constant("Cool").take(3).toList shouldBe Stream("Cool", "Cool", "Cool").toList
  }

  // Exercise5.9
  "increaising test" in {
    Stream.increasing.take(5).toList shouldBe Stream(0, 1, 2, 3, 4).toList
  }

  "increasing start from 13" in {
    Stream.from(13).take(3).toList shouldBe Stream(13, 14, 15).toList
  }

  // Exercise 5.10
  "fibo" in {
    Stream.fibs.take(5).toList shouldBe Stream(0, 1, 1, 2, 3).toList
  }

  // Exercise 5.11
  "unfold" in {
    Stream.unfold(0)(i => Some(i, i+1)).take(5).toList shouldBe List(0, 1, 2, 3, 4)
  }
  "fibo2" in {
    Stream.fibs2.take(5).toList shouldBe Stream(0, 1, 1, 2, 3).toList
  }

  "increaising test2" in {
    Stream.from2(0).take(5).toList shouldBe Stream(0, 1, 2, 3, 4).toList
  }

  "increasing start from 132" in {
    Stream.from2(13).take(3).toList shouldBe Stream(13, 14, 15).toList
  }

  "constant2" in {
    Stream.constant2("Wassupp!").take(5).toList shouldBe List("Wassupp!", "Wassupp!", "Wassupp!", "Wassupp!", "Wassupp!")
  }

  "ones2" in {
    Stream.ones2.take(100).toList shouldBe Stream.ones.take(100).toList
  }
}

