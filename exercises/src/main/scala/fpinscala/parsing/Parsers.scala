package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}

import language.higherKinds
import language.implicitConversions

//trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
//
//  case class ParserOps[A](p: Parser[A]) {}
//
//  object Laws {}
//
//  def char(ch: Char): Parser[Char]
//  def run[A](parser: Parser[A])(input: String): Either[ParseError, A]
//}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

//  def toError(msg: String): ParseError = {
//    ParseError(List(this -> msg))
//  }

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String = {
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
  }
}

//case class ParseError(
//  stack: List[(Location, String)] = List(),
//  otherFailures: List[ParseError] = List()
//)

trait Parsers[ParseError, Parser[+_]] { self =>

  def char(ch: Char): Parser[Char]
  def or[A](a: Parser[A], b: Parser[A]): Parser[A]
  def run[A](parser: Parser[A])(input: String): Either[ParseError, A]
  def listOfN[A](n: Int, parser: Parser[A]): Parser[List[A]]

  implicit def string(str: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = {
    ParserOps(f(a))
  }

  def many[A](parser: Parser[A]): Parser[List[A]]
  def map[A, B](parser: Parser[A])(f: A => B): Parser[B]

  case class ParserOps[A](parser: Parser[A]) {
    def |[B >: A](p: Parser[B]): Parser[B] = self.or(parser, p)
    def or[B >: A](p: => Parser[B]): Parser[B] = self.or(parser, p)

    def map[B](f: A => B): Parser[B] = self.map(parser)(f)
    def many: Parser[List[A]] = self.many(parser)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      ???
    }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
      ???
    }
  }
}