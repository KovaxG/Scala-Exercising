package Streaming

import Chapters.{None, Some, Option}

sealed trait Stream[+A] {
  def headOption2: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h()) // Something to do with the thunk
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => List(h()) ++ t().toList
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case _ if n == 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n-1))
    case Empty => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case stuff if n <= 0 => stuff
    case Cons(_, t) => t().drop(n-1)
    case Empty => Empty
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = {
    def c: (A, => Stream[A]) => Stream[A] = (b, a) => if(p(b)) Cons(() => b, () => a) else Empty
    this.foldRight(Empty: Stream[A])(c)
  }

  def headOption: Option[A] = {
    def OOR(o1: Option[A], o2: Option[A]): Option[A] = o2 match {
      case Some(b) => Some(b)
      case None => None
    }

    this.foldRight(None: Option[A])((a, b) => OOR(b, Some(a)))
  }

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (f(a)) Cons(() => a, () => b) else b)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = ones.map(_ => a)

  def increasing : Stream[Int] = Stream.cons(0, increasing.map(_+1))
  def from(n: Int): Stream[Int] = increasing.map(_+n)

  def fibs: Stream[Int] = {
    def loop(f1: Int, f2: Int): Stream[Int] = {
      Stream.cons(f1, loop(f2, f1 + f2))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def loop(s: S): Stream[A] = {
      f(s) match {
        case None => Empty
        case Some((a, s)) => Stream.cons(a, loop(s))
      }
    }

    loop(z)
  }

  type S = (Int, Int)
  def sum(s: S): Int = s._1 + s._2
  def fibs2: Stream[Int] = unfold((0, 1): S)(s => Some(s._1, (s._2, sum(s))): Option[(Int, S)])

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))

  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  val ones2: Stream[Int] = constant2(1)
}



