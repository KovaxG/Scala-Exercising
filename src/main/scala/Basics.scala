object Basics {

  // Exercise 2.1 Fibonacci
  def fib(n: Int): Int = {
    def loop(term: Int, result: Int, iter:Int): Int  =
      if (iter == n) term
      else loop(result, result + term, iter+1)

    loop(0, 1, 1)
  }

  // Exercise 2.2 isSorted
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(iter: Int, result: Boolean): Boolean =
      if (iter + 1 >= as.length) result
      else if (ordered(as(iter), as(iter+1))) loop(iter+1, result && true)
      else false // If we found a tuple that is not in order, then stop looping

    loop(0, true)
  }

  // Exercise 2.3 Currying
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => {(b: B) => f(a, b)}

  // Exercise 2.4 Uncurrying
  def uncurry[A, B, C](f: A => B => C): (A,B) => C = (a: A, b: B) => f(a)(b)

  // Exercise 2.5 Function Composition
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
