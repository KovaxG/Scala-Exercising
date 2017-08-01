package Parallel

import java.util.concurrent.{Callable, ExecutorService, TimeUnit, Future}

object Par {

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  type Par[A] = ExecutorService => Future[A]

  // Creates a computation that immediately results in a value a
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // Combines the results of two parallel computations with a binary function
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)
    UnitFuture(f(af.get , bf.get))
  }

  // Marks a computation for concurrent evaluation by run
  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(new Callable[A] {
      def call = a(es).get
    })

  // Wraps the expression a for concurrent evaluation by run
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  //Fully evaluates a given Par, spawning parallel computations
  // as requested by fork and extracting the resulting value
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


}
