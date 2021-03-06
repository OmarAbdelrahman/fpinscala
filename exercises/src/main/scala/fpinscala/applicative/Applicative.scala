package fpinscala
package applicative

import java.util.Date

import monads.Functor
import state._
import State._
import monoids._
import language.higherKinds
import language.implicitConversions
import scala.util.Try

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A]))((a, fas) => map2(a, fas)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if (n <= 0) unit(List.empty[A]) else map2(fa, replicateM(n - 1, fa))(_ :: _)

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)(_ -> _)

  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fn: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fn._1)(p._1), G.apply(fn._2)(p._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) {
      case ((key, fvalue), fmap) =>
        map2(fvalue, fmap)((value, map) => map + (key -> value))
    }

  def _map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  override def map[A,B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
}

object Monad {
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] = new Monad[({ type f[x] = Either[E, x] })#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Left(value)  => Left(value)
      case Right(value) => f(value)
    }
  }

  def stateMonad[S]: Monad[({ type f[x] = State[S, x] })#f] = new Monad[({ type f[x] = State[S, x] })#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st.flatMap(f)
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({ type f[x] = F[N[x]] })#f] = ???
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  type Const[A, B] = A

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.continually(a) // The infinite, constant stream
    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] = // Combine elements pointwise
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x] })#f] = new Applicative[({ type f[x] = Validation[E, x] })#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) =>
        Success(f(a, b))
      case (Failure(h1, t1), Failure(h2, t2)) =>
        Failure(h1, t1 ++ Vector(h2) ++ t2)
      case (failure @ Failure(_, _), _) =>
        failure
      case (_, failure @ Failure(_, _)) =>
        failure
    }
  }

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({ type f[x] = Const[M, x] })#f] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      override def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  def validName(name: String): Validation[String, String] = {
    if (name != "") Success(name)
    else Failure("Name can not be empty")
  }
  def validBirthdate(birthdate: String): Validation[String, Date] = {
    Try {
      import java.text._
      new SimpleDateFormat("yyyy-MM-dd").parse(birthdate)
    } match {
      case util.Failure(_) => Failure("Birthdate must be in the form yyyy-MM-dd")
      case util.Success(value) => Success(value)
    }
  }
  def validPhone(phoneNumber: String): Validation[String, String] = {
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits")
  }
  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] = {
    validationApplicative.map3(validName(name), validBirthdate(birthdate), validPhone(phone))(WebForm)
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  private val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(monoid: Monoid[B]): B =
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(monoidApplicative(monoid))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccumulate[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)(
      (a: A) =>
        for {
          s1 <- get[S]
          (b, s2) = f(a, s1)
          _ <- set(s2)
        } yield b
    ).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccumulate(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccumulate(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccumulate(fa, toList(fa).reverse)((_, list) => (list.head, list.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccumulate(fa, z)((a, b) => ((), f(b, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type f[x] = F[G[x]] })#f] = ???
}

object Traverse {
  val list: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  val option: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(value) => G.map(f(value))(Some(_))
        case None => G.unit(None)
      }
  }

  val tree: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), list.traverse(fa.tail)(tree => traverse(tree)(f)))(Tree(_, _))
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3)
    val x = list.reverse(a)
    println(x)
  }
}
