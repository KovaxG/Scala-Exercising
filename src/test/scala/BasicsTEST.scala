import org.scalatest.{FreeSpec, Matchers}

class BasicsTEST extends FreeSpec with Matchers {

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

}
