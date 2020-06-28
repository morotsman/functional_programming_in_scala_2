package chapter11

import chapter11.StateMonad.{getState, setState}
import chapter4.{None, Option, Some}
import chapter6.State
import chapter8.{Gen, Prop}
import org.scalatest.FunSuite


class StateMonadUsageTest extends FunSuite {


  test("calculate with state") {
    def plus(value: Int): State[Int, Unit] = for {
      state <- getState
      _ <- setState(state + value)
    } yield ()

    def times(value: Int): State[Int, Unit] = for {
      state <- getState
      _ <- setState(state * value)
    } yield ()

    val result = for {
      _ <- plus(1)
      _ <- plus(1)
      _ <- times(4)
    } yield ()

    assert(result.run(1) == ((), 12))
  }

  type Number = Int
  type WinningNumbers = List[Number]
  type MatchingNumbers = List[Number]

  test("lottery") {
    case class LotteryState(matchingNumbers: MatchingNumbers, winningNumbers: WinningNumbers)
    val lotteryMonad = Monad.stateMonad[LotteryState]

    def checkNumber(lotteryNumber: Number): State[LotteryState, Unit] =
      for {
        s <- getState
        _ <- setState(if (s.winningNumbers.contains(lotteryNumber)) s.copy(matchingNumbers = lotteryNumber :: s.matchingNumbers) else s)
      } yield ()

    def checkTicket(ticket: List[Number], winningNumbers: WinningNumbers): MatchingNumbers =
      lotteryMonad.sequence(ticket.map(checkNumber)).run(LotteryState(List(), winningNumbers))._2.matchingNumbers

    val ticket = List[Number](1, 5, 4, 34, 44, 15, 19)
    val winningNumbers: WinningNumbers = List(1, 4, 29, 5, 27, 21, 34)
    assert(checkTicket(ticket, winningNumbers) == List(34, 4, 5, 1))
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
