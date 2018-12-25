package fpinscala.state

object StateMain {
  def main(args: Array[String]): Unit = {
    val machine = Machine(locked = true, 5, 10)
    val input = List[Input](Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val ((coins, candies), newMachine) = Candy.simulateMachine(input).run(machine)
    println(coins + " " + candies)
    println(newMachine)
  }
}
