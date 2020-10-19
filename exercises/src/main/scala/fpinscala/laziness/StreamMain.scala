package fpinscala.laziness

import fpinscala.laziness.Stream._

object StreamMain {
  case class Id(value: Long)

  def main(args: Array[String]): Unit = {
    val s = "strawberries"
    println(s.substring(2, 5))
  }
}
