package fpinscala.errorhandling


import scala.util.{Failure, Success, Try}
import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMap1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def orElse1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def filter1(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  def map3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def map4[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def s1[A](a: List[Option[A]]): Option[List[A]] = {
    val init: Option[List[A]] = Some(List.empty[A])
    a.foldLeft(init) {
      case (None, _) => None
      case (_, None) => None
      case (x, Some(y)) => x.map(l => l :+ y)
    }
  }

  def sequenceWithFold[A](a: List[Option[A]]): Option[List[A]] = {
    val init: Option[List[A]] = Some(List.empty[A])
    a.foldLeft(init)((optList, as) => map2(optList, as)((list, av) => list ::: List(av)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(s => sequence(t).map(list => s :: list))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case ha :: listA => f(ha).flatMap(b => traverse(listA)(f).map(list => b :: list))
  }

  def traverseWithMap2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case ha :: listA => map2(f(ha), traverseWithMap2(listA)(f))(_ :: _)
  }

  def traverseWithFold[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val init: Option[List[B]] = Some(List.empty[B])
    a.foldLeft(init)((optList, av) => map2(optList, f(av))((list, bv) => list ::: List(bv)))
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(optA => optA)
  }

  def main(args: Array[String]): Unit = {
    val s = Seq(
      List(None, Some(1), Some(2)),
      List(Some(1), Some(2), Some(3)),
      List(),
      List(None),
      List(Some(1))
    )
    s.foreach(sq => println(sequence(sq) + " " + s1(sq) + " " + sequenceViaTraverse(sq) + " " + sequenceWithFold(sq)))
    val ss = Seq(
      List("1", "23", "omar"),
      List("1", "2", "3")
    )
    val f: String => Option[Int] = {
      s => Try(s.toInt) match {
        case Success(v) => Some(v)
        case Failure(_) => None
      }
    }
    ss.foreach(sq => println(traverse(sq)(f) + " " + traverseWithMap2(sq)(f) + " " + traverseWithFold(sq)(f)))
  }
}