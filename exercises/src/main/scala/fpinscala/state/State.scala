package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  val double: Rand[Double] = map(nonNegativeInt)(v => v / (Int.MaxValue.toDouble + 1))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeInt: Rand[Int] = rng => {
    val (v, newRng) = rng.nextInt
    val nonNegative = if (v < 0) -(v + 1) else v
    nonNegative -> newRng
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def double_1(rng: RNG): (Double, RNG) = {
    val (v, newRng) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    i -> d -> r2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    d -> i -> r2
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  def ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def intsWithRng(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) List() -> rng
    else {
      val (i, r1) = rng.nextInt
      val (j, r2) = intsWithRng(count - 1)(r1)
      (i :: j, r2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      f(a, b) -> r2
    }
  }

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)(_ -> _)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))((r, acc) => map2(r, acc)(_ :: _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
  }
  def mapWithoutFlatMap[B](f: A => B): State[S, B] = State(
    state => {
      val (a, nextState) = run(state)
      f(a) -> nextState
    }
  )
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    state => {
      val (a, nextState) = run(state)
      f(a).run(nextState)
    }
  )
  def both[B](sb: State[S, B]): State[S, (A, B)] = {
    map2(sb)(_ -> _)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = {
    State(state => a -> state)
  }

  def sequenceViaFoldRight[S, A](sequence: List[State[S, A]]): State[S, List[A]] = {
    sequence.foldRight(unit[S, List[A]](List.empty))((elem, acc) => elem.map2(acc)(_ :: _))
  }

  def sequence[S, A](sequence: List[State[S, A]]): State[S, List[A]] = {
    @annotation.tailrec
    def doIt(state: S, actions: List[State[S, A]], result: List[A]): (List[A], S) = actions match {
      case Nil =>
        result.reverse -> state
      case h :: t =>
        val (v, nextState) = h.run(state)
        doIt(nextState, t, v :: result)
    }
    State(state => doIt(state, sequence, List.empty[A]))
  }

  def sequenceViaFoldLeft[S, A](sequence: List[State[S, A]]): State[S, List[A]] = {
    sequence.reverse.foldLeft(unit[S, List[A]](List.empty))((acc, elem) => elem.map2(acc)(_ :: _))
  }

  def get[S]: State[S, S] = State(state => state -> state)
  def set[S](state: S): State[S, Unit] = State(_ => () -> state)

  def modify[S](f: S => S): State[S, Unit] = {
    for { state <- get } yield set(f(state))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
