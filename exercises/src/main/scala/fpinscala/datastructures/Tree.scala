package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size(t: Tree[A] forSome { type A }): Int = fold(t)(_ => 1)((l, r) => l + r + 1)

  def sizeOld(t: Tree[_]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = fold(t)(x => x)((l, r) => l max r)

  def maximumOld(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(t: Tree[_]): Int = fold(t)(_ => 0)((l, r) => l max r + 1)

  def depthOld(t: Tree[_]): Int = t match {
    case Branch(l, r) => (depth(l) max depth(r)) + 1
    case _ => 0
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))

  def mapOld[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}
