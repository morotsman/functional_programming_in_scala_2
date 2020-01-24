package chapter6

import org.scalatest._

class RNGSpec extends FlatSpec with Matchers {

  case class NotSoRandom(results: List[Int]) extends RNG {

    def nextInt: (Int, RNG) = {
      (results.head, NotSoRandom(results.tail))
    }
  }


  "SimpleRNG(1).nextInt" should "result in (384748,SimpleRNG(25214903928L))" in {
    assert(SimpleRNG(1).nextInt == (384748, SimpleRNG(25214903928L)))
  }

  "randomPair" should "result two random ints" in {
    val rng = NotSoRandom(List(1, 3))
    assert(RNG.randomPair(rng)._1 == (1, 3))
  }

  "nonNegativeInt" should "result in non negative ints" in {
    val rng = NotSoRandom(List(-1, -2, 3))
    assert(rng.nextInt._1 == -1)
    assert(RNG.nonNegativeInt(rng)._1 == 3)
  }

  "nonNegativeInt" should "handle Int.minValue" in {
    val rng = NotSoRandom(List(-1, Int.MinValue, 3))
    assert(rng.nextInt._1 == -1)
    assert(RNG.nonNegativeInt(rng)._1 == 3)
  }

  "double" should "result in double" in {
    val rng = NotSoRandom(List(0, Int.MaxValue, Int.MaxValue / 2))

    val (zero, rng1) = RNG.double(rng)
    assert(zero == 0)

    val (max, rng2) = RNG.double(rng1)
    assert(max == 1.0)

    val (half, rng3) = RNG.double(rng2)
    assert(half == 0.49999999976716936)
  }

  "intDouble" should "result in int, double" in {
    val rng = NotSoRandom(List(Int.MaxValue, Int.MaxValue / 2))

    val ((max, half), rng1) = RNG.intDouble(rng)
    assert(max == Int.MaxValue)
    assert(half == 0.49999999976716936)
  }

  "doubleInt" should "result in int, double" in {
    val rng = NotSoRandom(List(Int.MaxValue, Int.MaxValue / 2))

    val ((max, half), rng1) = RNG.doubleInt(rng)
    assert(half == Int.MaxValue)
    assert(max == 0.49999999976716936)
  }

  "ints" should "generate a number of ints" in {
    val rng = NotSoRandom(List(0, 1, 2, 3, 4, 5))


    assert(RNG.ints(0)(rng)._1 == List())
    assert(RNG.ints(1)(rng)._1 == List(0))
    assert(RNG.ints(6)(rng)._1 == List(0, 1, 2, 3, 4, 5).reverse)
  }


  "doubleInTermsOfMap" should "result in double" in {
    val rng = NotSoRandom(List(0, Int.MaxValue, Int.MaxValue / 2))

    val (zero, rng1) = RNG.doubleInTermsOfMap()(rng)
    assert(zero == 0)

    val (max, rng2) = RNG.doubleInTermsOfMap()(rng1)
    assert(max == 1.0)

    val (half, rng3) = RNG.doubleInTermsOfMap()(rng2)
    assert(half == 0.49999999976716936)
  }

  "map2" should "map two rng's" in {
    val rng1 = NotSoRandom(List(1, 2))
    val (result, nextRng) = RNG.map2(rng => rng.nextInt, rng => rng.nextInt)(_ + _)(rng1)
    assert(result == 3)

  }

  "sequence" should "result in a list of randomnes" in {
    val rng = NotSoRandom(List(1, 2, 3))

    val (result1, _) = RNG.sequence(List())(rng)
    assert(result1 == List())

    val (result2, _) = RNG.sequence(List((rng: RNG) => rng.nextInt))(rng)
    assert(result2 == List(1))

    val (result3, _) = RNG.sequence(List((rng: RNG) => rng.nextInt, (rng: RNG) => rng.nextInt, (rng: RNG) => rng.nextInt))(rng)
    assert(result3 == List(1, 2, 3))
  }


  "ints" should "generate a list of random ints" in {
    val rng = NotSoRandom(List(1, 2, 3))

    assert(RNG.intsInTermsOfSequence(0)(rng)._1 == List())
    assert(RNG.intsInTermsOfSequence(1)(rng)._1 == List(1))
    assert(RNG.intsInTermsOfSequence(3)(rng)._1 == List(1, 2, 3))
  }

  "flatMap" should "map and flatten" in {
    val rng = NotSoRandom(List(1, 2))

    val (pair, _) = RNG.flatMap(RNG.int)(a => RNG.map(RNG.int)(b => (a, b)))(rng)
    assert(pair == (1, 2))
  }

  "nonNegativeLessThan" should "generate numbers below a limit" in {

    val rng = NotSoRandom(List(-1, 10, 5, -3, 4))

    assert(RNG.nonNegativeLessThan(11)(rng)._1 == 10)
    assert(RNG.nonNegativeLessThan(10)(rng)._1 == 5)
    assert(RNG.nonNegativeLessThan(5)(rng)._1 == 4)
  }

  "map2InTermsOfFlatMap" should "map two rng's" in {
    val rng1 = NotSoRandom(List(1, 2))
    val (result, nextRng) = RNG.map2InTermsOfFlatMap(rng => rng.nextInt, rng => rng.nextInt)(_ + _)(rng1)
    assert(result == 3)
  }

  "mapInTermsOfFlatMap" should "map one rng" in {
    val rng1 = NotSoRandom(List(1, 2))
    val (result, nextRng) = RNG.mapInTermsOfFlatMap(rng => rng.nextInt)(_ + 2)(rng1)
    assert(result == 3)
  }

  "unit" should "add something to the state context" in {
    val (a, s) = State.unit(1).run("ignored state")
    assert(a == 1)
    assert(s == "ignored state")
  }

  "map" should "map something in the state context" in {
    val (a, s) = State.unit(1).map(_ + 2).run("ignored state")
    assert(a == 3)
    assert(s == "ignored state")
  }


  "map2" should "applicative map  something in the state context" in {
    val (a, s) = State.unit(1).map2(State.unit[String, Int](1))(_ + _).run("ignored state")
    assert(a == 2)
    assert(s == "ignored state")
  }

  "flatMap" should "should chain context aware functions" in {
    val (a, s) = State.unit(1).flatMap(a => State.unit[String, (Int, Int)]((a, a + 1))).run("ignored state")
    assert(a == (1, 2))
    assert(s == "ignored state")
  }

  "sequence" should "sequence" in {
    val (a, s) = State.sequence[String, Int](List(State.unit(1), State.unit(2), State.unit(3))).run("ignored state")
    assert(a == List(1, 2, 3))
    assert(s == "ignored state")
  }

  "get" should "should return the state" in {
    val (a, s) = State.unit(1).get.run("state")
    assert(a == "state")
    assert(s == "state")
  }

  "set" should "should replace state with new state" in {
    val (a, s) = State.unit(1).set("another state").run("state")
    assert(a == ())
    assert(s == "another state")
  }

  "no input" should "leave machine as is" in {
    val simulation = Machine.simulateMachine(List())
    val result = simulation.run(Machine(true, 10, 0))
    assert(result._1 == (0, 10))
    assert(result._2 == Machine(true, 10, 0))
  }

  "coin when locked" should "unlocked machine" in {
    val simulation = Machine.simulateMachine(List(Coin))
    val result = simulation.run(Machine(true, 10, 0))
    assert(result._1 == (1, 10))
    assert(result._2 == Machine(false, 10, 1))
  }

  "coin when unlocked" should "do nothing" in {
    val simulation =
      Machine.simulateMachine(List(Coin, Coin))
    val result = simulation.run(Machine(true, 10, 0))
    assert(result._1 == (1, 10))
    assert(result._2 == Machine(false, 10, 1))
  }

  "turn when locked" should "do nothing" in {
    val simulation = Machine.simulateMachine(List(Turn))
    val result = simulation.run(Machine(true, 10, 0))
    assert(result._1 == (0, 10))
    assert(result._2 == Machine(true, 10, 0))
  }

  "turn when unlocked" should "dispense candy" in {
    val simulation = Machine.simulateMachine(List(Coin, Turn))
    val result = simulation.run(Machine(true, 10, 0))
    assert(result._1 == (1, 9))
    assert(result._2 == Machine(true, 9, 1))
  }

  "a machine with no candy" should "ignore input" in {
    val simulation = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn))
    val result = simulation.run(Machine(true, 1, 0))
    assert(result._1 == (1, 0))
    assert(result._2 == Machine(true, 0, 1))
  }

  "10 coins and 5 candies" should "result in (14,1) if 4 candies are bought" in {
    val simulation = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
    val result = simulation.run(Machine(true, 5, 10))
    assert(result._1 == (14, 1))
    assert(result._2 == Machine(true, 1, 14))
  }


}
