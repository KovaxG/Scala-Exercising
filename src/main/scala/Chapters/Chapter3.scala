package Chapters

import DTList._

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
    case Cons(_, xs) => xs
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
  def append[A](to: List[A], that: List[A]): List[A] = List.foldRight(to, that)(Cons(_,_))

  // Exercise 3.15: Flatten
  def flatten[A](xss: List[List[A]]): List[A] = List.foldRight(xss, Nil: List[A])(append)

  // Exercise 3.16: Add1
  def add1(xs: List[Int]): List[Int] = List.foldRight(xs, Nil: List[Int])((a, b) => Cons(a+1, b))

  // Exercise 3.17: MapToString
  def mapToString(ds: List[Double]): List[String] = List.foldRight(ds, Nil: List[String])((a, b) => Cons(a.toString, b))

  // Exercise 3.18: Map
  def map[A,B](as: List[A])(f: A => B): List[B] = List.foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // Exercise 3.19: Filter
  def filter[A](as: List[A])(f: A => Boolean): List[A] = List.foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  // Exercise 3.20: flatMap
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  // Exercise 3.21: newFilter
  def newFilter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 3.22: Add coresponding
  def head[A](as: List[A]): A = as match {
    case Nil => ???
    case Cons(h, _) => h
  }

  def addCoresponding(as: List[Int], bs: List[Int]): List[Int] = {
    def loop(l1: List[Int], l2: List[Int], result: List[List[Int]]): List[Int] =
      if (l1 == Nil) map(result)(l => List.sum(l))
      else loop(tail(l1), tail(l2), append(result, List(List(head(l1), head(l2)))))

    loop(as, bs, Nil)
  }

  // Exercise 3.23: zipWith
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    def f2: A => B => C = Chapter2.curry(f)

    val as2: List[B => C] = map(as)(f2)

    def loop(xs: List[B => C], ys: List[B], result: List[C]): List[C] =
      if (xs == Nil) result
      else loop(tail(xs), tail(ys), append(result, List(head(xs)(head(ys)))))

    loop(as2, bs, Nil)
  }

  // Exercise 3.24: hasSubsequence
  class State[A](val found: Boolean, val started: Boolean, val list: List[A], val sublist: List[A]) {}

  def hasSequence[A](sup: List[A], sub: List[A]): Boolean = {

    val default = new State(false, false, Nil, sub)

    def isDef: State[A] => Boolean = s => s == default

    def combinator(_state: State[A], value: A): State[A] = _state  match {
      case state if !state.found && state.sublist == Nil => new State(true, false, state.list, Nil) // Found
      case state if state.found => state // Skip all because found
      case state if isDef(state) && value == head(sub) => new State(false, true, List(value), tail(sub)) // Start Recording
      case state if value == head(state.sublist) && state.started && !state.found => new State(state.found, state.started, append(state.list, List(value)), tail(state.sublist)) // Nominal
      case state if value != head(state.sublist) && value == head(sub) && state.started && !state.found => new State(false, true, List(value), tail(sub)) // Wrong value, but you can restart
      case state if value != head(state.sublist) && state.started && !state.found => default // Wrong value
      case state if !state.started && value != head(state.sublist) => default // not found, not started
      case _ => ??? // this should never happen
    }

    val result = List.foldLeft(sup, default)(combinator)

    result.found
  }
}



