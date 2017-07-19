import org.scalatest.{FreeSpec, Matchers}

class BasicsTEST extends FreeSpec with Matchers {

  // Exercise 2.1 Fibonacci
  "first number" in {
    Basics.fib(1) shouldBe 0
  }

  "second number" in {
    Basics.fib(2) shouldBe 1
  }

  "third nr" in {
    Basics.fib(3) shouldBe 1
  }

  "fourth nr" in {
    Basics.fib(4) shouldBe 2
  }

  "fifth" in {
    Basics.fib(5) shouldBe 3
  }

  "sixth" in {
    Basics.fib(6) shouldBe 5
  }

  // Exercise 2.2 isSorted
  def intOrder(a: Int, b: Int): Boolean = a < b

  "empty list" in {
    Basics.isSorted(Array(), intOrder) shouldBe true
  }

  "single element" in {
    Basics.isSorted(Array(5), intOrder) shouldBe true
  }

  "two elements in order" in {
    Basics.isSorted(Array(2,5), intOrder) shouldBe true
  }

  "two elements not in order" in {
    Basics.isSorted(Array(5, 1), intOrder) shouldBe false
  }

  "random array in order" in {
    Basics.isSorted(Array(1, 3, 4, 5, 6, 10, 101), intOrder) shouldBe true
  }

  "inorder in the middle" in {
    Basics.isSorted(Array(1, 3, 4, 5, 98, 6, 10, 101), intOrder) shouldBe false
  }

  "inorder in the beginning" in {
    Basics.isSorted(Array(100, 1, 3, 4, 5, 6, 10, 101), intOrder) shouldBe false
  }

  "inorder in the end" in {
    Basics.isSorted(Array(1, 3, 4, 5, 6, 10, 101, 0), intOrder) shouldBe false
  }

  // Exercise 2.3 Currying
  "randomCurryTest" in {
    // Now that I think about it, if it isn't correct, I can not compile the code, so yay!
    def testFunc: Int => (Int => Int) = Basics.curry((a: Int, b: Int) => a + b)
    def testFunc2: Int => Int = testFunc(5)
    def result = testFunc2(1)
    result shouldBe 6
  }

  // Exercise 2.4 Uncurrying
  "randomUnCurryTest" in {
    def f: Int => Int => Int = a => b => a + b

    def fPrime: (Int, Int) => Int = Basics.uncurry(f)

    def result = fPrime(1, 2)

    result shouldBe 3
  }

  // Exercise 2.5 Function Composition
  "randomCompositionTest" in {
    def f1: Boolean => Int = b => if (b) 10 else 0
    def f2: Int => String = i => if (i == 0) "Zero" else "Some"

    def F: Boolean => String = Basics.compose(f2, f1)

    F(false) shouldBe "Zero"
  }
}
