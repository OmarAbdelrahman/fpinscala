package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}

import language.higherKinds
import language.implicitConversions

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError = {
    ParseError(List(this -> msg))
  }

  def advanceBy(n: Int): Location = {
    copy(offset = offset + n)
  }

  /* Returns the line corresponding to this location */
//  def currentLine: String = {
//    if (input.length > 1) input.lines.drop(line - 1).next
//    else ""
//  }

  def columnCaret: String = (" " * (col - 1)) + "^"
}

case class ParseError(stack: List[(Location, String)] = List())

trait Parsers[Parser[+ _]] { self =>
  def char(ch: Char): Parser[Char] = {
    string(ch.toString).map(_.charAt(0))
  }
  def succeed[A](a: A): Parser[A] = {
    string("").map(_ => a)
  }
  def or[A](a: Parser[A], b: Parser[A]): Parser[A]
  def run[A](parser: Parser[A])(input: String): Either[ParseError, A]
  def listOfN[A](n: Int, parser: Parser[A]): Parser[List[A]]
  def many[A](parser: Parser[A]): Parser[List[A]] = {
    parser.map2(parser.many)(_ :: _) | succeed(List())
  }
  def manyOrOne[A](parser: Parser[A]): Parser[List[A]] = {
    parser.map2(parser.many)(_ :: _)
  }
  def map[A, B](parser: Parser[A])(f: A => B): Parser[B]
  def flatMap[A, B](parser: Parser[A])(f: A => Parser[B]): Parser[B]
  def slice[A](parser: Parser[A]): Parser[String]
  def product[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] = {
    a.map2(b)(_ -> _)
  }
  def map2[A, B, C](a: Parser[A], b: Parser[B])(f: (A, B) => C): Parser[C] = {
    a.flatMap(ap => b.map(bp => f(ap, bp)))
  }

  implicit def string(string: String): Parser[String]
  implicit def operators[A](parser: Parser[A]): ParserOps[A] = ParserOps[A](parser)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = {
    ParserOps(f(a))
  }

  case class ParserOps[A](parser: Parser[A]) {
    def |[B >: A](p: Parser[B]): Parser[B] = self.or(parser, p)
    def or[B >: A](p: => Parser[B]): Parser[B] = self.or(parser, p)
    def map[B](f: A => B): Parser[B] = self.map(parser)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(parser)(f)
    def many: Parser[List[A]] = self.many(parser)
    def many1: Parser[List[A]] = self.manyOrOne(parser)
    def slice: Parser[String] = self.slice(parser)
    def product[B](b: Parser[B]): Parser[(A, B)] = self.product(parser, b)
    def **[B](b: Parser[B]): Parser[(A, B)] = self.product(parser, b)
    def map2[B, C](b: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(parser, b)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop = {
      Prop.forAll(input)(string => run(p1)(string) == run(p2)(string))
    }
    def mapLaw[A](p: Parser[A])(input: Gen[String]): Prop = {
      equal(p, p.map(a => a))(input)
    }
    def succeedLaw[A](p: Parser[A])(input: Gen[String]): Prop = {
      Prop.forAll(input)(string => run(succeed("something"))(string) == Right("something"))
    }
    def productLaw[A, B](a: Parser[A], b: Parser[B])(input: Gen[String]): Prop = {
      //Prop.forAll(input)(string => run(a | b)(string) == run(a ** b)(string))
      ???
    }
  }
}
