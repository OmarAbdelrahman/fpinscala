package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum[A](tree: Tree[A])(gt: (A, A) => A): A = {
    tree match {
      case Leaf(x) => x
      case Branch(l, r) => gt(maximum(l)(gt), maximum(r)(gt))
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeWithFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(1 + _ + _)
  }

  def maximumWithFold(tree: Tree[Int]): Int = {
    fold(tree)(a => a)(_ max _)
  }

  def depthWithFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 0)((a, b) => 1 + (a max b))
  }

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }

  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(5), Leaf(6)), Branch(Leaf(9), Leaf(1)))
    println(size(t) + " " + sizeWithFold(t))
    println(maximum(t)(_ max _) + " " + maximumWithFold(t))
    println(depth(t) + " " + depthWithFold(t))
    println(map(t)(_ + 2) + " " + mapWithFold(t)(_ + 2))
  }
}