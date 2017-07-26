package Chapters

import DTList._
import org.scalatest.{FreeSpec, Matchers}

class Chapter4Test extends FreeSpec with Matchers {

  // Exercise 4.1: The Option Datatype
  // Map
  "map succes" in {
    Some(15).map(a => a / 3) shouldBe Some(5)
  }

  "map fail" in {
    val test: Option[Int] = None
    test.map(a => a + 1) shouldBe None
  }

  // FlatMap
  "flatMap fail" in {
    val test: Option[Int] = None
    test.flatMap(a => Some(a - 1)) shouldBe None
  }

  "flatMap map none" in {
    Some(8).flatMap(a => None) shouldBe None
  }

  "flatMap nominal" in {
    Some("Hello").flatMap(s => if(s.length < 10) Some("Hello World") else None) shouldBe Some("Hello World")
  }

  // GerOrElse
  "OrElse" in {
    None.getOrElse(42) shouldBe 42
  }

  "Get" in {
    Some(12).getOrElse(42) shouldBe 12
  }

  // Or Else
  "or" in {
    None.orElse(Some(42)) shouldBe Some(42)
  }

  "else" in {
    Some(12).orElse(Some(42)) shouldBe Some(12)
  }

  // Filter
  "nominal" in {
    Some(42).filter(a => a > 100) shouldBe None
  }

  "filter not" in {
    Some(42).filter(a => a < 100) shouldBe Some(42)
  }

  // Exercise 4.2
  "variance on empty seq" in {
    Chapter4.variance(Seq(): Seq[Double]) shouldBe None
  }

  "non empty variance" in {
    Chapter4.variance(Seq(1, 2, 3, 4, 5)) shouldBe Some(2)
  }

  // Exercise 4.3: Map2 test
  "both are None" in {
    val a: Option[Int] = None
    val b = None
    Chapter4.map2(a, b)(_+_) shouldBe None
  }

  "A is None" in {
    val a: Option[Int] = None
    val b = Some(2)
    Chapter4.map2(a, b)(_+_) shouldBe None
  }

  "B is None" in {
    val a = Some(1)
    val b = None
    Chapter4.map2(a, b)(_+_) shouldBe None
  }

  "both are ok" in {
    val a = Some(1)
    val b = Some(2)
    Chapter4.map2(a, b)(_+_) shouldBe Some(3)
  }

  // Exercise 4.4: Sequence
  "There are no Nones" in {
    Chapter4.sequence(List(Some(1), Some(2), Some(3), Some(4))) shouldBe Some(List(1, 2, 3, 4))
  }

  "There is a None" in {
    Chapter4.sequence(List(Some(1), Some(2), None, Some(4))) shouldBe None
  }

  // Exercise 4.5: Traverse
  "Seq there are no Nones" in {
    Chapter4.traverse(List(1, 2, 3, 4))(a => Some(a)) shouldBe Some(List(1, 2, 3, 4))
  }

  "Traverse There is a None" in {
    Chapter4.traverse(List(1, 2, 3, 4))(a => None) shouldBe None
  }

  // Exercise 4.7: Sequence
  "There are no Lefts" in {
    Chapter4.esequence(List(Right(1), Right(2), Right(3), Right(4))) shouldBe Right(List(1, 2, 3, 4))
  }

  "There is a Left" in {
    Chapter4.esequence(List(Left("LEFT"), Right(4), Left("DEAD"), Right(2))) shouldBe Left("DEAD")
  }

  // Exercise 4.5: Traverse
  "Seq there are no Lefts" in {
    Chapter4.etraverse(List(1, 2, 3, 4))(a => Right(a)) shouldBe Right(List(1, 2, 3, 4))
  }

  "Traverse There is a Left" in {
    Chapter4.etraverse(List(1, 2, 3, 4))(_ => Left("DEAD")) shouldBe Left("DEAD")
  }
}