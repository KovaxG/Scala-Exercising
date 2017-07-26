package Chapters

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25
  def size[A](fa: Tree[A]): Int = fa match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(fa: Tree[Int]): Int = fa match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](fa: Tree[A]): Int = fa match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](fa: Tree[A])(b: (B,B) => B)(f: A => B): B = fa match {
    case Leaf(v) => f(v)
    case Branch(l, r) => b(fold(l)(b)(f), fold(r)(b)(f))
  }
}