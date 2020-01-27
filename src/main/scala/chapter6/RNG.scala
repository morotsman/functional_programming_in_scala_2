package chapter6

import chapter3.{Cons, List}

trait RNG {

  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  val boolean: Rand[Boolean] =
    map(double)(d => if (d > 0.5) true else false)

  val char: Rand[Char] =
    map(between(97, 122))(i => i.toChar)

  private def charsToString(chars: List[Char]): String = chars.mkString("")

  val string: Rand[String] = {
    val chars: Rand[List[Char]] = flatMap(between(0, 20))(i => sequence(List.fill(i)(char)))
    map(chars)(charsToString)
  }

  val sentence: Rand[String] = {
    val words = flatMap(between(0, 20))(i => sequence(List.fill(i)(string)))
    map(words)(w => w.mkString(" "))
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)


  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  def randomPair2(): Rand[(Int, Int)] =
    rng => {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng2) = rng.nextInt
    if (i1 > -1) (i1, rng2)
    else nonNegativeInt(rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng2) = nonNegativeInt(rng)
    (i1.toDouble / Int.MaxValue.toDouble, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def go(c: Int, acc: List[Int], rng: RNG): (List[Int], RNG) =
      if (c == 0) (acc, rng)
      else {
        val (i, rng2) = rng.nextInt
        go(c - 1, Cons(i, acc), rng2)
      }

    go(count, List(), rng)
  }

  def between(start: Int, stopExclusive: Int)(rng: RNG): (Int, RNG) = {
    val (result, rng2) = double(rng)
    val value = start + (result * (stopExclusive - start)).toInt
    (value.toInt, rng2)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleInTermsOfMap(): Rand[Double] =
    map(nonNegativeInt)(i => i / Int.MaxValue.toDouble)

  def map2[A, B, C](ar: Rand[A], br: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ar(rng)
      val (b, rng3) = br(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](as: List[Rand[A]]): Rand[List[A]] =
    as.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)((aa, bb) => Cons(aa, bb)))

  def intsInTermsOfSequence(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(a => if (a < n) unit(a) else nonNegativeLessThan(n))

  def map2InTermsOfFlatMap[A, B, C](ar: Rand[A], br: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ar)(a => map(br)(b => f(a, b)))

  def mapInTermsOfFlatMap[A, B](as: Rand[A])(f: A => B): Rand[B] =
    flatMap(as)(a => unit(f(a)))
}


case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](bs: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => bs.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a: A, s1: S) = this.run(s)
    f(a).run(s1)
  })

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))

  def modify(f: S => S): State[S, Unit] = for (
    s <- get;
    _ <- set(f(s))
  ) yield ()


}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] =
    as.foldRight(State.unit(List()): State[S, List[A]])((a, b) => b.map2(a)((aa, bb) => Cons(bb, aa)))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for (
    s <- get;
    _ <- set(f(s))
  ) yield ()

}


sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def simulateMachine(input: List[Input]): State[Machine, (Int, Int)] = State { m =>
    val machine: Machine = input.foldLeft(m)(rule)
    ((machine.coins, machine.candies), machine)
  }

  private def rule(machine: Machine, input: Input): Machine = input match {
    case Coin =>
      if (machine.candies == 0) {
        machine
      } else if (machine.locked) {
        val unlocked = false
        Machine(unlocked, machine.candies, machine.coins + 1)
      } else {
        machine
      }
    case Turn =>
      if (machine.candies == 0) {
        machine
      } else if (!machine.locked) {
        val locked = true
        Machine(locked, machine.candies - 1, machine.coins)
      } else {
        machine
      }
  }

}
