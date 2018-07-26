package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(er) => Left(er)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => f(a)
      case Left(er) => Left(er)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      eA <- this
      eB <- b
    } yield f(eA, eB)
  }

  def map3[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(eA => b.map(eB => f(eA, eB)))
  }
}

case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h :: t => f(h).flatMap(b => traverse(t)(f).map(listB => b :: listB))
    }
  }

  def traverseWithMap2[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }
  }

  def traverseWithFold[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val init: Either[E, List[B]] = Right(List.empty[B])
    es.foldLeft(init)((b, a) => f(a).map2(b)((a, b) => b ::: List(a)))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(e => e)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list!") else Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def safeDiv2(x: Int, y: Int): Either[Exception, Int] = {
    Try(x / y)
  }

  def main(args: Array[String]): Unit = {
    val ss = Seq(
      List("1", "23", "omar"),
      List("1", "2", "3")
    )
    ss.foreach { seq =>
      println(traverse(seq)(s => Try(s.toInt)) + " " + traverseWithFold(seq)(s => Try(s.toInt)) + " " + traverseWithMap2(seq)(s => Try(s.toInt)))
    }
  }
}