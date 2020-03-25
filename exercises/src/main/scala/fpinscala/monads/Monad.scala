package fpinscala
package monads

import fpinscala.parallelism.Par.Par
import parsing._
import testing._
import parallelism._
import state._

import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }

  val genFunctor: Functor[Gen] = new Functor[Gen] {
    override def map[A, B](gen: Gen[A])(f: A => B): Gen[B] = gen.map(f)
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]

  def map[A, B](a: M[A])(f: A => B): M[B] =
    flatMap(a)(a => unit(f(a)))
  def map2[A, B, C](a: M[A], b: M[B])(f: (A, B) => C): M[C] =
    flatMap(a)(a => map(b)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    lma.foldRight(unit(List[A]()))((ma, list) => map2(ma, list)(_ :: _))
  }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(unit(List[B]()))((a, list) => map2(f(a), list)(_ :: _))
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
    map2(ma, mb)(_ -> _)
  }

  def filterM[A](list: List[A])(f: A => M[Boolean]): M[List[A]] = {
    list match {
      case Nil => unit(Nil)
      case h :: t =>
        flatMap(f(h))(b => map(filterM(t)(f))(list => if (b) h :: list else list))
    }
  }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => {
    flatMap(f(a))(g)
  }

  def _sequence[A](lma: List[M[A]]): M[List[A]] = {
    lma match {
      case Nil => unit(Nil)
      case h :: t => flatMap(h)(m => map(_sequence(t))(list => m :: list))
    }
  }

  def _traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b => map(_traverse(t)(f))(list => b :: list))
    }
  }

  def _replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)
  }

  def _filterM[A](list: List[A])(f: A => M[Boolean]): M[List[A]] = {
    list match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b => if (!b) filterM(t)(f) else map(filterM(t)(f))(h :: _))
    }
  }

  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    compose((_: Unit) => ma, f)(())
  }

  def join[A](mma: M[M[A]]): M[A] = {
    flatMap(mma)(ma => ma)
  }

  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    join(map(ma)(f))
  }

  def _compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => {
    join(map(f(a))(g))
  }
}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](a: Gen[A])(f: A => Gen[B]): Gen[B] = a.flatMap(f)
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(a)(f)
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)
    override def flatMap[A, B](a: P[A])(f: A => P[B]): P[B] = p.flatMap(a)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = a.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](a: Stream[A])(f: A => Stream[B]): Stream[B] = a.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a.flatMap(f)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = a.flatMap(f)
  }

  def stateMonad[S]: Monad[({type f[X] = State[S, X]})#f] = new Monad[({ type f[X] = State[S, X] })#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](a: State[S, A])(f: A => State[S, B]): State[S, B] = a.flatMap(f)
  }

  def readerMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](a: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = a.flatMap(f)
  }

  val F: Monad[({ type f[X] = State[Int, X] })#f] = stateMonad[Int]

  def zipWithIndex[A](list: List[A]): List[(Int, A)] = {
    val state = list.foldLeft(F.unit(List[(Int, A)]())) { (acc, a) =>
      for {
        xs <- acc
        n  <- State.get
        _  <- State.set(n + 1)
      } yield (n, a) :: xs
    }
    state.run(0)._1.reverse
  }

  def main(args: Array[String]): Unit = {
    println(zipWithIndex(List(1, 2, 3, 4)))
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))
  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(run(r)).run(r))
}

object Reader {
  def ask[R]: Reader[R, R] = Reader(r => r)
}
