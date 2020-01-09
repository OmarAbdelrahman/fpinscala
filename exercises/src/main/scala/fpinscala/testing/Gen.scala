package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x               => x
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x                 => x
    }
  }

  def tag(msg: String): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(failedCase, successCount) =>
        Falsified(msg + "\n" + failedCase, successCount)
      case x => x
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type MaxSize = Int
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }
  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def apply(f: (TestCases, RNG) => Result): Prop = {
    Prop((_, n, rng) => f(n, rng))
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = {
    forAll(g(_))(f)
  }

  def forAll[A](gen: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(gen(i))(f))
    val prop: Prop = props
      .map { p =>
        Prop((max, _, rng) => p.run(max, casesPerSize, rng))
      }
      .toList
      .reduce(_ && _)
    prop.run(max, n, rng)
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(gen)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map(toTestResult(f))
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }

  def run(
    p: Prop,
    maxSize: MaxSize = 100,
    testCases: TestCases = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)
  ): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, count) =>
        println(s"! Falsified after $count passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }

  private def toTestResult[A](f: A => Boolean): ((A, SuccessCount)) => Result = {
    case (a, i) =>
      try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = {
    Gen(State(RNG.unit(a)))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(r => start + r % (stopExclusive - start)))
  }

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(gen.sample)))
  }

  def listOf[A](gen: Gen[A]): SGen[List[A]] = {
    SGen(n => listOfN(n, gen))
  }

  def listOf1[A](gen: Gen[A]): SGen[List[A]] = {
    SGen(n => listOfN(n max 1, gen))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(sample.map2(g.sample)(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(listSize => Gen.listOfN(listSize, this))
  }

  def listOf: SGen[List[A]] = {
    Gen.listOf(this)
  }

  def listOf1: SGen[List[A]] = {
    Gen.listOf1(this)
  }

  def unSized: SGen[A] = {
    SGen(_ => this)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = {
    forSize(n)
  }

  def map[B](f: A => B): SGen[B] = {
    SGen(n => forSize(n).map(f))
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g: Int => Gen[B] = n => {
      forSize(n).flatMap(f(_).forSize(n))
    }
    SGen(g)
  }
}
