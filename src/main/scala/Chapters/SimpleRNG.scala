package Chapters

trait RNG {
  def nextInt: (Int, RNG)

  def nextDouble: (Double, RNG) = {
    val (x, rnd2) = this.nextInt
    val numberBetween0and1 = x.toDouble / Int.MaxValue.toDouble
    (numberBetween0and1, rnd2)
  }

  def ints(count: Int): (List[Int], RNG) = {
    val zero: (List[Int], RNG) = (List() , this)
    def comb(a: (List[Int], RNG), b: Int): (List[Int], RNG) = {
      val (i, rng) = a._2.nextInt
      val list = a._1
      (list ++ List(i), rng)
    }

    (1 to count).foldLeft(zero)(comb)
  }
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def intDouble: ((Int, Double), RNG) = {
    val (i, rnd2) = this.nextInt
    val (d, rnd3) = rnd2.nextDouble
    ((i, d), rnd3)
  }

  def doubleInt: ((Double, Int), RNG) = {
    val (d, rnd2) = this.nextDouble
    val (i, rnd3) = rnd2.nextInt
    ((d, i), rnd3)
  }

  def double3: ((Double, Double, Double), RNG) = {
    val (d1, rng1) = this.nextDouble
    val (d2, rng2) = rng1.nextDouble
    val (d3, rng3) = rng2.nextDouble
    ((d1, d2, d3), rng3)
  }
}

case object SimpleRNG {

  type Rand[A] = RNG => (A, RNG)

  def positivize(a: Int): Int = math.abs(a >>> 2)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_,_))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnd2) = ra(rng)
      val (b, rnd3) = rb(rnd2)

      (f(a, b), rnd3)
    }

  def _int: Rand[Int] = rng => rng.nextInt

  def double: Rand[Double] = map(_int)(_.toDouble / Int.MaxValue.toDouble)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng  => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt: Rand[Int] = rng => {
    val (nni, rnd2) = rng.nextInt
    (SimpleRNG.positivize(nni), rnd2)

  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List()): Rand[List[A]])((a: Rand[List[A]], b: Rand[A]) => map2(a, b)((lista, a) => lista ++ List(a)))

  // Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, seed) = f(rng)
    val (b, seed2) = g(a)(seed)
    (b, seed2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  })

  def mapfm[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  // Exercise 6.9
  def map2fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
}
