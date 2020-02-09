package chapter13

import chapter12.Monad2
import chapter13.Free.Console._
import chapter13.Free._
import chapter6.{State}
import chapter3.List
import chapter4.{Either, Left, Right}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  private def rule(machine: Machine, input: Input): Either[String, Machine] = input match {
    case Coin =>
      if (machine.candies == 0) {
        Left("No candies Left")
      } else if (machine.locked) {
        val unlocked = false
        Right(Machine(unlocked, machine.candies, machine.coins + 1))
      } else {
        Left("Could not accept coin, turn to get a candy")
      }
    case Turn =>
      if (machine.candies == 0) {
        Left("No candies Left")
      } else if (!machine.locked) {
        val locked = true
        Right(Machine(locked, machine.candies - 1, machine.coins))
      } else {
        Left("You need to dispose a coin to get a candy")
      }
  }

  def candyProgram(): Free[Console, State[Either[String, Machine], Unit]] =
    for {
      - <- printLn("Input: c(coin) or t(turn)")
      input <- readLn
    } yield (State[Either[String, Machine], Unit] { em: Either[String, Machine] => {
      val machine: Either[String, Machine] = em.flatMap(m => rule(m, input.map(i => if (i == "c") Coin else Turn).get))
      ((), machine)
    }
    })

  def errorInfoProgram(message: String): Free[Console, Unit] =
    printLn("Error: " + message)

  def currentStatusProgram(m: Machine): Free[Console, Unit] = m match {
    case Machine(locked, candies, coins) if locked => printLn(s"The machine is locked and has $candies candies left")
    case Machine(locked, candies, coins) => printLn(s"The machine is unlocked and has $candies candies left")
  }

  def input(machine: Machine): Free[Console, Either[String, Machine]] = for {
    oldMachine <- freeMonad.unit(machine)
    _ <- printLn("")
    _ <- currentStatusProgram(oldMachine)
    program <- candyProgram().map(a => a.run(Right(oldMachine)))
  } yield (program._2)

  def candyMachine(machine: Machine): Unit = {
      runConsole(input(machine)) match {
      case Left(message) =>
        runConsole(errorInfoProgram("Error: " + message))
        candyMachine(machine)
      case Right(m) =>
        candyMachine(m)
        m
    }
  }

  def main(args: Array[String]): Unit = {
    //candyMachine(Machine(true, 10, 0))

    val res = runConsoleState(input(Machine(true, 10, 0))).run(Buffers(List("c"), List()))
    res._2.out.reverse().forEach(a => println(a))
    println(res)

    val res2 = runConsoleState(input(Machine(false, 10, 0))).run(Buffers(List("c"), List()))
    res2._2.out.reverse().forEach(a => println(a))
    println(res2)
  }
}
