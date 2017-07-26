package Chapters

import DTList._

object Chapter4 {
  // Exercise 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.length == 0) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def c: (Option[List[A]], Option[A]) => Option[List[A]] = (acc, v) => v flatMap (value => acc map (list => Chapter3.append(list, List(value))))
    val z: Option[List[A]] = Some(List())

    List.foldLeft(a, z)(c)
  }

  // Exercise 4.5
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def c: (Option[List[B]], A) => Option[List[B]] = (acc, v) => f(v) flatMap (value => acc.map(list => Chapter3.append(list, List(value))))
    val z: Option[List[B]] = Some(List())

    List.foldLeft(a, z)(c)
  }

  // Exercise 4.7
  def esequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    def c: (Either[E, List[A]], Either[E, A]) => Either[E, List[A]] = (acc, v) => v flatMap (value => acc map (list => Chapter3.append(list, List(value))))
    val z: Either[E, List[A]] = Right(List())

    List.foldLeft(es, z)(c)
  }

  def etraverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def c: (Either[E, List[B]], A) => Either[E, List[B]] = (acc, v) => f(v) flatMap (value => acc.map(list => Chapter3.append(list, List(value))))
    val z: Either[E, List[B]] = Right(List())

    List.foldLeft(as, z)(c)
  }
}
