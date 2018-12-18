package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val someListResult = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((acc, e) => Cons(e, acc))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def fSum(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x, y) => x * y`; see sidebar

  def fProduct(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("List is empty.")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("List is empty.")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, b) => 1 + b)

  def fLength[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, List[A]())((acc, h) => Cons(h, acc))

  def reverse2[A](list: List[A]): List[A] =
    foldLeftWithFoldRight(list, List[A]())((acc, h) => Cons(h, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  def map2[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((a, b) => Cons(f(a), b))
  }

  def foldRightWithFoldLeftAndReverse[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(list), z)((a, b) => f(b, a))
  }

  def concat[A](list: List[List[A]]): List[A] = {
    foldRight(list, List[A]())(append3)
  }

  def foldLeftWithFoldRight[A, B](list: List[A], z: B)(f: (B, A) => B): B = ???

  def foldRightWithFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B = ???

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    }
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]()) { (a, b) =>
      if (f(a)) Cons(a, b) else b
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((a, b) => append(f(a), b))
  }

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def flatMap3[A, B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => List[B]()
      case Cons(x, xs) => append3(f(x), flatMap3(xs)(f))
    }
  }

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(e => if (f(e)) List(e) else Nil)
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (Cons(a, at), Cons(b, bt)) => Cons(f(a, b), zipWith(at, bt)(f))
      case _ => Nil
    }
  }

  @tailrec
  def startsWith[A](list: List[A], prefix: List[A]): Boolean = {
    (list, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }
  }

  @tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubSequence(t, sub)
    }
  }

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3)
    val y = List(4, 5)
    println(zipWith(x, y)(_ + _))
  }
}
