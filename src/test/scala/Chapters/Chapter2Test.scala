package Chapters

import org.scalatest.{FreeSpec, Matchers}

// Chapter 2: Getting Started with Functional Programming
class Chapter2Test extends FreeSpec with Matchers {

  // Exercise 2.1 Fibonacci
  "first number" in {
    Chapter2.fib(1) shouldBe 0
  }

  "second number" in {
    Chapter2.fib(2) shouldBe 1
  }

  "third nr" in {
    Chapter2.fib(3) shouldBe 1
  }

  "fourth nr" in {
    Chapter2.fib(4) shouldBe 2
  }

  "fifth" in {
    Chapter2.fib(5) shouldBe 3
  }

  "sixth" in {
    Chapter2.fib(6) shouldBe 5
  }

  // Exercise 2.2 isSorted
  def intOrder(a: Int, b: Int): Boolean = a < b

  "empty list" in {
    Chapter2.isSorted(Array(), intOrder) shouldBe true
  }

  "single element" in {
    Chapter2.isSorted(Array(5), intOrder) shouldBe true
  }

  "two elements in order" in {
    Chapter2.isSorted(Array(2,5), intOrder) shouldBe true
  }

  "two elements not in order" in {
    Chapter2.isSorted(Array(5, 1), intOrder) shouldBe false
  }

  "random array in order" in {
    Chapter2.isSorted(Array(1, 3, 4, 5, 6, 10, 101), intOrder) shouldBe true
  }

  "inorder in the middle" in {
    Chapter2.isSorted(Array(1, 3, 4, 5, 98, 6, 10, 101), intOrder) shouldBe false
  }

  "inorder in the beginning" in {
    Chapter2.isSorted(Array(100, 1, 3, 4, 5, 6, 10, 101), intOrder) shouldBe false
  }

  "inorder in the end" in {
    Chapter2.isSorted(Array(1, 3, 4, 5, 6, 10, 101, 0), intOrder) shouldBe false
  }

  // Exercise 2.3 Currying
  "randomCurryTest" in {
    // Now that I think about it, if it isn't correct, I can not compile the code, so yay!
    def testFunc: Int => (Int => Int) = Chapter2.curry((a: Int, b: Int) => a + b)
    def testFunc2: Int => Int = testFunc(5)
    def result = testFunc2(1)
    result shouldBe 6
  }

  // Exercise 2.4 Uncurrying
  "randomUnCurryTest" in {
    def f: Int => Int => Int = a => b => a + b

    def fPrime: (Int, Int) => Int = Chapter2.uncurry(f)

    def result = fPrime(1, 2)

    result shouldBe 3
  }

  // Exercise 2.5 Function Composition
  "randomCompositionTest" in {
    def f1: Boolean => Int = b => if (b) 10 else 0
    def f2: Int => String = i => if (i == 0) "Zero" else "Some"

    def F: Boolean => String = Chapter2.compose(f2, f1)

    F(false) shouldBe "Zero"
  }
}
