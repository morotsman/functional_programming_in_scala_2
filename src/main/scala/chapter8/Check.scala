package chapter8

import chapter5.Stream
import chapter6.{RNG, SimpleRNG, State}
import chapter8.Gen.unit
import chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(_))(f)

  private def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max + 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      val prop: Prop =
        props.map(p => Prop(
          (max, _, rng) => {
            p.run(max, casesPerSize, rng)
          }
        )).toList.reduce(_ && _)

      prop.run(max, n, rng)
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (maxSize, n, rng) => {
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMessage(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMessage[A](s: A, e: Exception): String =
    s"test case: ${s}\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
        throw new RuntimeException("Test failed")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop(
    (maxSize, testcases, rng) => {
      val result1: Result = this.run(maxSize, testcases, rng)
      val result2: Result = p.run(maxSize, testcases, rng)
      (result1, result2) match {
        case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified("[" + f2 + "]", s1 + s2)
        case (Falsified(_, _), _) => result1
        case (_, Falsified(_, _)) => result2
        case _ => Passed
      }
    }
  )

  def ||(p: Prop): Prop = Prop(
    (maxSize, testcases, rng) => {
      val result1: Result = this.run(maxSize, testcases, rng)
      val result2: Result = p.run(maxSize, testcases, rng)
      (result1, result2) match {
        case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(f1, s1 + s2)
        case (Falsified(_, _), _) => result2
        case (_, Falsified(_, _)) => result1
        case _ => Passed
      }
    }
  )
}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(State(
    rng => {
      val (a, rng1) = this.sample.run(rng)
      f(a).sample.run(rng1)
    }
  ))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => Gen.listOfN(s, this))

  def unsized[B >: A](): SGen[B] = SGen(
    size => this
  )

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.between(start, stopExclusive)))

  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def int: Gen[Int] =
    Gen(State(RNG.int))

  def char: Gen[Char] =
    Gen(State(RNG.char))

  def string: Gen[String] =
    Gen(State(RNG.string))

  def pairOfInt: Gen[(Int, Int)] =
    Gen(State(RNG.randomPair2()))

  def optionOf[A](as: Gen[A]): Gen[Option[A]] =
    boolean.flatMap(b => as.map(a => if (b) Some(a) else None))

  def chapter4OptionOf[A](as: Gen[A]): Gen[chapter4.Option[A]] =
    boolean.flatMap(b => as.map(a => if (b) chapter4.Some(a) else chapter4.None))

  def endoFunction[A]: Gen[A => A] =
    int.map(_ => a => a)


  def map2[A, B, C](as: Gen[A], bs: Gen[B])(f: (A, B) => C): Gen[C] =
    as.flatMap(a => bs.map(b => f(a, b)))

  def sequence[A](as: List[Gen[A]]): Gen[List[A]] =
    as.foldLeft(unit(List[A]()))((a, acc) => map2(a, acc)((aa, bb) => bb :: aa))

  def sequence[A](as: Stream[Gen[A]]): Gen[Stream[A]] =
    as.foldRight(unit(Stream[A]()))((acc, a) => map2(a, acc)((aa, bb) => Stream.cons(bb, aa)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    sequence(List.fill(n)(g))

  def streamOfN[A](n: Int, g: Gen[A]): Gen[Stream[A]] =
    sequence(Stream.unfold(g)(g => Some(g, g)).take(n))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double.flatMap(d => if (d < g1._2) g1._1 else g2._1)

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => listOfN(size, g))

  def sentence(): SGen[String] =
    SGen(size => listOfN(size,Gen.string).map(s => s.mkString(" ")))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => listOfN(size + 1, g))

  def streamOf[A](g: Gen[A]): SGen[Stream[A]] =
    SGen(size => streamOfN(size, g))
}

case class SGen[+A](forSize: Int => Gen[A])
