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

  //translation of example given in Programming in Haskell by Graham Hutton on page 171
  test("relabling tree") {
    import StateMonad.getState
    import StateMonad.setState

    trait Tree[A]
    case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]
    case class Leaf[A](a: A) extends Tree[A]

    val tree = Node(Node(Leaf("a"), Leaf("b")), Leaf("c"))

    def rlabel[A](a: Tree[A], acc: Int): (Tree[Int], Int) = a match {
      case Leaf(_) => (Leaf(acc), acc + 1)
      case Node(l, r) => {
        val (l1, acc1) = rlabel(l, acc)
        val (r1, acc2) = rlabel(r, acc1)
        (Node(l1, r1), acc2)
      }
    }

    assert(rlabel(tree, 0)._1 == Node(Node(Leaf(0),Leaf(1)),Leaf(2)))

    def fresh: State[Int, Int] = for {
      n <- getState
      _ <- setState(n + 1)
    } yield n

    def rlabel2[A](a: Tree[A]): State[Int, Tree[Int]] = a match {
      case Leaf(_) => fresh.map(Leaf(_))
      case Node(l, r) => for {
        l1 <- rlabel2(l)
        r1 <- rlabel2(r)
      } yield Node(l1, r1)
    }

    assert(rlabel2(tree).run(0)._1 == Node(Node(Leaf(0),Leaf(1)),Leaf(2)))

  }

}
