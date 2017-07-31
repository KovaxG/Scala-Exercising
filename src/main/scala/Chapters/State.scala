package Chapters


case class State[S,+A](run: S => (A, S))
object State {
  type State[S, +A] = S => (A, S)

  def unit[A, S](a: A): State[S, A] = s => (a, s)

  def map[A, B, S](a: State[S, A])(f: A => B): State[S, B] =
    s => {
      val (a2, s2) = a(s)
      (f(a2), s2)
    }

  def map2[A, B, C, S](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    s => {
      val (a, s2) = sa(s)
      val (b, s3) = sb(s2)
      (f(a, b), s3)
    }

  def flatMap[A, B, S](a: State[S, A])(f: A => State[S, B]): State[S, B] =
    s => {
      val (a2, s2) = a(s)
      f(a2)(s2)
    }

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] = fs.foldLeft(unit(List()): State[S, List[A]])((a, v) => map2(a, v)((lista, lm) => lista ++ List(lm)))
}

