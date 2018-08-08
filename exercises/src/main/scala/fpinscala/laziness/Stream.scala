package fpinscala.laziness

import Stream._

import scala.collection.mutable.ListBuffer

trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }
  }

  def forAllWithFold(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) if n > +1 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileWithFold(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, _) if !p(h()) => false
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionWithFold: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, acc) => cons(f(a), acc))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    ???
  }

  def toListRecursive: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @annotation.tailrec
    def doIt(stream: Stream[A], result: List[A]): List[A] = stream match {
      case Empty => result
      case Cons(h, t) => doIt(t(), h() :: result)
    }
    doIt(this, List.empty).reverse
  }

  def toListIterative: List[A] = {
    val buf = ListBuffer.empty[A]
    @annotation.tailrec
    def doIt(stream: Stream[A]): List[A] = stream match {
      case Empty => buf.toList
      case Cons(h, t) => buf += h(); doIt(t())
    }
    doIt(this)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = {
    Empty
  }
  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = {
    Stream.cons(1, ones)
  }

  def from(n: Int): Stream[Int] = {
    ???
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    ???
  }

  def main(args: Array[String]): Unit = {
    val stream = cons(1, cons(2, cons(3, cons(4, cons(5, empty)))))

    println(stream.toListRecursive)
    println(stream.toList)
    println(stream.toListIterative)

    println(stream.drop(3).toListIterative)
    println(stream.take(3).toListIterative)

    println("take while 1 = " + stream.takeWhile(_ < 3).toListIterative)
    println("take while 2 = " + stream.takeWhileWithFold(_ < 3).toListIterative)

    println(stream.forAll(_ < 6))
    println(stream.forAll(_ < 3))

    println(stream.headOption)
    println(stream.headOptionWithFold)

    println(stream.map(_ * 2).toListIterative)
    println(stream.map(_ + 5).toListIterative)

    println("filter 1 = " + stream.filter(_ % 2 == 0).toListIterative)
    println("filter 2 = " + stream.filter(_ % 2 != 0).toListIterative)
  }
}