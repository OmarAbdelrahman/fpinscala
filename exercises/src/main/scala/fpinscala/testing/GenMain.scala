package fpinscala.testing

import fpinscala.testing.Prop.forAll

object GenMain {
  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
    val sortedProp = forAll(Gen.listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.isEmpty || sorted.tail.isEmpty || sorted.zip(sorted.tail).forall { case (a, b) => a <= b }
    }
    Prop.run(sortedProp)
  }
}
