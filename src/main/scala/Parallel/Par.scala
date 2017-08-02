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

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)
  //map2(parList, unit(()))((a, _) => a.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldLeft(unit(Nil): Par[List[A]])((a, b) => map2(a, b)((x, y) => y :: x))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(
      parMap(as)(a => (f(a), a))
    )( _.foldLeft(Nil: List[A])((a, b) => if (b._1) b._2 :: a else a))

  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    def f2: (A, B) => C => D = (a, b) => c => f(a, b, c)
    map2(map2(a, b)(f2), c)((g, e) => g(e))
  }
}
