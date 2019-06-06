package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => xs
    case Nil => throw new RuntimeException("empty list")
  }

  def tail2[A](l: List[A]): Option[List[A]] = l match {
    case Cons(x, xs) => Some(xs)
    case Nil => None
  }

  def tail3[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => xs
    case Nil => l
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, l) => Cons(h, l)
    case _ => throw new RuntimeException("empty list")
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    import scala.collection.mutable
    val buf = new mutable.ListBuffer[A]
    @annotation.tailrec
    def fillBuffer(ys: List[A]): Unit = ys match {
      case Cons(_, Nil) | Nil => Nil
      case Cons(x, xs) =>
        buf += x
        fillBuffer(xs)
    }
    fillBuffer(l)
    List(buf.toList: _*)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => 1 + b)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldSum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def foldProduct(ints: List[Double]): Double = foldLeft(ints, 1.0)(_ * _)

  def foldLength(ints: List[Int]): Int = foldLeft(ints, 0)((acc, _) => acc + 1)

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, List[A]())((acc, x) => Cons(x, acc))

  def foldLeftWithFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((x, g) => b => g(f(b, x)))(z)

  def foldRightWithFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, x) => b => g(f(x, b)))(z)

  def foldRightWithFoldLeftAndReverse[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc, x) => f(x, acc))

  def foldAppend[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def concat[A](xs: List[List[A]]): List[A] = foldLeft(xs, Nil: List[A])(append)

  def add1(xs: List[Int]): List[Int] = foldRight(xs, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  def doubleToString(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    import scala.collection.mutable
    val buf = mutable.ListBuffer[B]()
    @annotation.tailrec
    def loop(xs: List[A]): Unit = xs match {
      case Cons(y, ys) =>
        buf += f(y)
        loop(ys)
      case _ => Nil
    }
    loop(l)
    List(buf.toList: _*)
  }

  def filter[A](xs: List[A])(p: A => Boolean): List[A] = {
    import scala.collection.mutable
    val buf = mutable.ListBuffer[A]()
    @annotation.tailrec
    def loop(ys: List[A]): Unit = ys match {
      case Cons(z, zs) =>
        if (p(z)) {
          buf += z
        }
        loop(zs)
      case _ => Nil
    }
    loop(xs)
    List(buf.toList: _*)
  }

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = concat(map(xs)(f))

  def filterWithFlatMap[A](xs: List[A])(p: A => Boolean): List[A] =
    flatMap(xs)(x => if (p(x)) List(x) else Nil)

  def addListsOfInt(l1: List[Int], l2: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(acc: List[Int], p: (List[Int], List[Int])): List[Int] = p match {
      case (Cons(x, xs), Cons(y, ys)) => loop(Cons(x + y, acc), (xs, ys))
      case _ => acc
    }

    reverse(loop(Nil, (l1, l2)))
  }
}
