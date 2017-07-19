object Basics {
  def fib(n: Int): Int = {
    def loop(term: Int, result: Int, iter:Int): Int  =
      if (iter == n) term
      else loop(result, result + term, iter+1)

    loop(0, 1, 1)
  }
}
