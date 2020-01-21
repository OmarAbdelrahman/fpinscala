package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override val zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def dual[A](monoid: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = monoid.op(a2, a1)
    override def zero: A = monoid.zero
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
    override def zero: A => A = a => a
  }

  def composeMonoid[A]: Monoid[A => A] = endoMonoid[A]
  def andThenMonoid[A]: Monoid[A => A] = dual(composeMonoid)

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](monoid: Monoid[A], gen: Gen[A]): Prop = {
    val tupledGenerator = for {
      x <- gen
      y <- gen
      z <- gen
    } yield {
      (x, y, z)
    }
    val associativityProp = Prop.forAll(tupledGenerator) {
      case (a, b, c) =>
        monoid.op(monoid.op(a, b), c) == monoid.op(a, monoid.op(b, c))
    }
    val identityProp = Prop.forAll(gen) { a =>
      monoid.op(monoid.zero, a) == a && monoid.op(a, monoid.zero) == a
    }
    associativityProp && identityProp
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], monoid: Monoid[A]): A = {
    as.foldLeft(monoid.zero)(monoid.op)
  }

  def foldMap[A, B](as: List[A], monoid: Monoid[B])(f: A => B): B = {
    as.foldLeft(monoid.zero)((b, a) => monoid.op(b, f(a)))
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, composeMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, andThenMonoid[B])(a => b => f(b, a))(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], monoid: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) monoid.zero
    else if (as.length == 1) f(as.head)
    else {
      val (left, right) = as.splitAt(as.length / 2)
      monoid.op(foldMapV(left, monoid)(f), foldMapV(right, monoid)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    type MightBeOrderedRange = Option[(Int, Int, Boolean)]
    val monoid = new Monoid[MightBeOrderedRange] {
      override def op(a1: MightBeOrderedRange, a2: MightBeOrderedRange): MightBeOrderedRange = {
        (a1, a2) match {
          case (Some((x1, y1, b1)), Some((x2, y2, b2))) =>
            Some((x1 min x2, y1 max y2, b1 && b2 && y1 <= x2))
          case (value, None) => value
          case (None, value) => value
          case _ => None
        }
      }
      override def zero: MightBeOrderedRange = None
    }
    foldMapV(ints, monoid)(value => Some((value, value, true))).forall(_._3)
  }

  sealed trait WC
  object WC {
    def apply(char: Char): WC = {
      if (char.isWhitespace) Part("", 0, "") else Stub(char.toString)
    }
    def apply(string: String): WC = {
      foldMapV(string.toIndexedSeq, wcMonoid)(WC.apply)
    }
  }
  case class Stub(string: String)                           extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](monoid: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(monoid.op)
    override def zero: Par[A] = Par.unit(monoid.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], monoid: Monoid[B])(f: A => B): Par[B] = {
    Par
      .parMap(v)(f)
      .flatMap { bs =>
        foldMapV(bs, par(monoid))(b => Par.lazyUnit(b))
      }
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (Stub(a), Stub(b)) => Stub(a + b)
        case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
        case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          val nW = if ((r1 + l2).isEmpty) 0 else 1
          Part(l1, w1 + nW + w2, r2)
      }
    }
    override def zero: WC = Stub("")
  }

  def count(string: String): Int = {
    val evaluate: String => Int = { string =>
      string.length min 1
    }
    WC(string) match {
      case Stub(string) => evaluate(string)
      case Part(l, w, r) => evaluate(l) + w + evaluate(r)
    }
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(ab1: (A, B), ab2: (A, B)): (A, B) = {
      val (a1, b1) = ab1
      val (a2, b2) = ab2
      a.op(a1, a2) -> b.op(b1, b2)
    }
    override def zero: (A, B) = a.zero -> b.zero
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(f1: A => B, f2: A => B): A => B = a => b.op(f1(a), f2(a))
    override def zero: A => B = _ => b.zero
  }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = {
      (m1.keySet ++ m2.keySet).foldLeft(zero) { (map, k) =>
        map.updated(k, v.op(m1.getOrElse(k, v.zero), m2.getOrElse(k, v.zero)))
      }
    }
    override def zero: Map[K, V] = Map[K, V]()
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
  }
}

trait Foldable[F[_]] {
  //import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as)(f.curried)(Monoid.composeMonoid)(z)
  }
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as)(a => (b: B) => f(b, a))(Monoid.andThenMonoid)(z)
  }
  def foldMap[A, B](as: F[A])(f: A => B)(monoid: Monoid[B]): B = {
    foldLeft(as)(monoid.zero)((b, a) => monoid.op(f(a), b))
  }
  def concatenate[A](as: F[A])(monoid: Monoid[A]): A = {
    foldLeft(as)(monoid.zero)(monoid.op)
  }
  def toList[A](as: F[A]): List[A] = {
    foldRight(as)(List[A]())(_ :: _)
  }
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }
  override def foldMap[A, B](as: List[A])(f: A => B)(monoid: Monoid[B]): B = {
    foldLeft(as)(monoid.zero)((b, a) => monoid.op(b, f(a)))
  }
  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(monoid: Monoid[B]): B = {
    Monoid.foldMapV(as, monoid)(f)
  }
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(monoid: Monoid[B]): B = {
    as match {
      case Leaf(value) => f(value)
      case Branch(left, right) => monoid.op(foldMap(left)(f)(monoid), foldMap(right)(f)(monoid))
    }
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case Leaf(value) => f(z, value)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(monoid: Monoid[B]): B = {
    as match {
      case None => monoid.zero
      case Some(a) => f(a)
    }
  }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case None => z
      case Some(a) => f(z, a)
    }
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case None => z
      case Some(a) => f(a, z)
    }
  }
}

object MonoidExample {
  def main(args: Array[String]): Unit = {
    val productMonoid = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
    println(ListFoldable.foldMap(List(1, 2, 3, 4))(a => (1, a))(productMonoid))
  }
}
