import org.scalatest.{FreeSpec, Matchers}
import fp.ch3._

class Chapter3Test extends FreeSpec with Matchers {
  // Exercise 3.1 Pattern Matching
  "matchingStuff" in {
    Chapter3.matchingStuff shouldBe 3
  }

  // Exercise 3.2 Tail
  "emptylist" in {
    Chapter3.tail(Nil) shouldBe Nil
  }

  "nonemptyList" in {
     Chapter3.tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
  }

  // Exercise 3.3 setHead
  "setHeadtoEmptyList" in {
    Chapter3.setHead(1, Nil) shouldBe List(1)
  }

  "setHeadtoNonemptylist" in {
    Chapter3.setHead(1, List(2, 3, 4)) shouldBe List(1, 2, 3 ,4)
  }

  // Exercise 3.4 Drop
  "dropNill" in {
    Chapter3.drop(Nil, 3) shouldBe Nil
  }

  "dropZeroList" in {
    Chapter3.drop(List(1, 2 ,3), 0) shouldBe List(1, 2, 3)
  }

  "nominalDrop" in {
    Chapter3.drop(List(1, 2, 3), 2) shouldBe List(3)
  }

  "negativeDrop" in {
    Chapter3.drop(List(1, 2, 3), -3) shouldBe List(1, 2, 3)
  }

  "tooBigDrop" in {
    Chapter3.drop(List(1, 2, 3), 10) shouldBe Nil
  }

  // Exercise 3.5 DropWhile
  "dropWhileEmpty" in {
    Chapter3.dropWhile(Nil, (a: Int) => a > 2) shouldBe Nil
  }

  "nominalCaseDropWhile" in {
    Chapter3.dropWhile(List(1, 2, 3, 4, 5, 6), (a: Int) => a < 3) shouldBe List(3, 4, 5, 6)
  }

  "dropNothing" in {
    Chapter3.dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a > 100) shouldBe List(1, 2, 3, 4, 5)
  }

  // Exercise 3.6 Init
  "init Nil" in {
    Chapter3.init(Nil) shouldBe Nil
  }

  "init single" in {
    Chapter3.init(List(1)) shouldBe Nil
  }

  "init nominal" in {
    Chapter3.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
  }

  "init short" in {
    Chapter3.init(List(1, 2)) shouldBe List(1)
  }

  // Exercise 3.8: Constructor?
  "does it build a list?" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
  }

  // Exercise 3.9: Length
  "calculate the length" in {
    List.foldRight(List(1, 2, 3, 4), 0)((_, b) => b + 1) shouldBe 4
  }

  // Exercise 3.11: Sum, Product and Length
  "foldLeft length" in {
    List.foldLeft(List(1, 2, 3, 4), 0)((b, _) => b + 1) shouldBe 4
  }

  "foldLeft sum" in {
    List.foldLeft(List(1, 2, 3, 4), 0)(_+_) shouldBe 10
  }

  "foldLeft product" in {
    List.foldLeft(List(1, 2, 3, 4), 1)(_*_) shouldBe 24
  }

  // Exervise 3.12: Reverse
  "reverse some numbers" in {
    Chapter3.reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
  }

  "reverse nil" in {
    Chapter3.reverse(Nil) shouldBe Nil
  }

  // Exercise 3.13: Flip-Flop
  "flipflop sum" in {
    Chapter3.foldLeftRight(List(1, 2, 3, 4), 0)(_+_) shouldBe 10
  }

  "flipflop reverse" in {
    Chapter3.foldLeftRight(List(1, 2, 3, 4, 5), Nil: List[Int])((a, b) => Cons(b, a))
  }
}
