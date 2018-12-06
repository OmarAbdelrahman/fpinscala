package fpinscala.laziness

import Stream._
import scala.{Stream => _}
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

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold(this, n) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), ns) if ns > 1 => Some((h(), (t(), ns - 1)))
      case _ => None
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
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

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()) -> t())
      case _ => None
    }
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold(this, s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), t1() -> t2()))
      case _ => None
    }
  }

  def zip[B](s: Stream[B]): Stream[(A, B)] =
    zipWith(s)(_ -> _)

  def zipWithAll[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold(this, s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), t1() -> t2())
      case (Cons(h, t), Empty) => Some(f(Some(h()), None), t() -> empty[B])
      case (Empty, Cons(h, t)) => Some(f(None, Some(h())), empty[A] -> t())
      case (Empty, Empty) => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    zipWithAll(s)(_ -> _)
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)
  }

  def append[B >: A](stream: => Stream[B]): Stream[B] = {
    foldRight(stream)(cons(_, _))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])(f(_).append(_))
  }

  def findWithFilter(p: A => Boolean): Option[A] = {
    filter(p).headOptionWithFold
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

  val ones: Stream[Int] = constantViaUnfold(1)

  val fibs: Stream[Int] = {
    def doIt(a: Int, b: Int): Stream[Int] = {
      cons(a, doIt(b, a + b))
    }
    doIt(0, 1)
  }

  val fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) {
      case (a, b) => Some(a, (b, a + b))
    }
  }

  def constant_1[A](a: A): Stream[A] = {
    Stream.cons(a, constant_1(a))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val result: Stream[A] = Cons(() => a, () => result)
    result
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(v => Some(v, v + 1))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(a => Some(a, a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).fold(empty[A]) { next =>
      val (a, s) = next
      cons(a, unfold(s)(f))
    }
  }
}