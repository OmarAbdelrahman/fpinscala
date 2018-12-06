package fpinscala.laziness

import fpinscala.laziness.Stream._

object StreamMain {
  def main(args: Array[String]): Unit = {
    val a = fromViaUnfold(2)
    val b = a.mapViaUnfold(_ * 2)

    println(a.take(5).toListIterative)
    println(b.take(5).toListIterative)
    println(a.zipWith(b)(_ + _).take(5).toListIterative)
    println(a.take(5).zipAll(b.take(3)).take(5).toListIterative)
  }
}
