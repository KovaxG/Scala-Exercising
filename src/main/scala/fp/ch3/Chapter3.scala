package fp.ch3

// Chapter 3: Functional Data Structures
object Chapter3 {

  // Exercise 3.1 Pattern Matching
  def matchingStuff: Int = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
  }

  // Exercise 3.2: Tail
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // Exercise 3.3: setHead
  def setHead[A](h: A, as: List[A]): List[A] = Cons(h, as)

  //Exercise 3.4: Drop
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n-1)

  // Exercise 3.5 DropWhile
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if(f(h)) dropWhile(t, f) else Cons(h, t)
  }

  // Exercise 3.6 Init
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // Exercise 3.12: Reverse
  def reverse[A](xs: List[A]) = List.foldLeft(xs, Nil: List[A])((v, a) => Cons(a,v))

  // Exercise 3.13: FoldLeft as FoldRight
  def foldRightLeft[A,B](xs: List[A], z: B)(f: (B, A) => B): B = List.foldRight(xs, z)((a, b) => f(b, a))
  def foldLeftRight[A,B](xs: List[A], z: B)(f: (A, B) => B): B = List.foldLeft(xs, z)((a, b) => f(b, a))

  // Exercise 3.14: Append
}



