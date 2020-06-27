package chapter11

import chapter4.{None, Option, Some}
import chapter6.State
import chapter8.{Gen, Prop}
import org.scalatest.FunSuite


class StateMonadeUsageTest extends FunSuite {

  test("lottery") {
    sealed trait Input
    case class Number(n: Int) extends Input
    case class LotteryTicket(ns: List[Int], numberOfMatches: Int)

    def check: Input => LotteryTicket => LotteryTicket = (i: Input) => (l: LotteryTicket) => {
      (i, l) match {
        case (Number(n), LotteryTicket(ns, numberOfMatches)) if ns.contains(n) => LotteryTicket(ns, numberOfMatches + 1)
        case _ => l
      }
    }

    val input = List(Number(1), Number(4), Number(29), Number(5), Number(27), Number(21), Number(34))
    val inputSeq: List[State[LotteryTicket, (Number, Int)]] = input.map((i: Number) => State((s: LotteryTicket) => {
      val ticket = check(i)(s)
      ((i, ticket.numberOfMatches), ticket)
    }))

    val lotteryTicket = LotteryTicket(List(1, 6, 5, 23, 45, 27), 0)
    val lotteryMonad = Monad.stateMonad[LotteryTicket]

    assert(lotteryMonad.sequence(inputSeq).run(lotteryTicket)._2 == LotteryTicket(List(1, 6, 5, 23, 45, 27), 3))
    assert(lotteryMonad.sequence(inputSeq).run(lotteryTicket)._1 ==
      List((Number(1), 1), (Number(4), 1), (Number(29), 1), (Number(5), 2), (Number(27), 3), (Number(21), 3), (Number(34), 3)))
  }

  test("zipWithIndex") {
    import StateMonad.getState
    import StateMonad.setState

    val stateM = Monad.stateMonad[Int]

    def zipWithIndex[A](as: List[A]): List[(Int, A)] =
      as.foldLeft(stateM.unit(List[(Int, A)]())) { (acc, a) =>
        for {
          xs <- acc
          n <- acc.get
          _ <- acc.set(n + 1)
        } yield (n, a) :: xs
      }.run(0)._1.reverse

    def zipWithIndex2[A](as: List[A]): List[(Int, A)] =
      as.foldLeft(stateM.unit(List[(Int, A)]())) { (acc, a) =>
        for {
          xs <- acc
          n <- getState
          _ <- setState(n + 1)
        } yield (n, a) :: xs
      }.run(0)._1.reverse


    assert(zipWithIndex(List("a", "b", "c")) == List((0, "a"), (1, "b"), (2, "c")))
    assert(zipWithIndex2(List("a", "b", "c")) == List((0, "a"), (1, "b"), (2, "c")))
  }

}
