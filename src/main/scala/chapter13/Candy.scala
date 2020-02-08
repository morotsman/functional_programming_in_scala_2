package chapter13

import chapter12.Monad2
import chapter13.Free.Console._
import chapter13.Free._
import chapter6.{Machine, State}
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

  def candyProgram(): Free[Console, State[Either[String, Machine], (Int, Int)]] =
    for {
      - <- printLn("Input: c(coin) or t(turn)")
      input <- readLn
    } yield (State[Either[String, Machine], (Int, Int)] { em: Either[String, Machine] => {
      val machine: Either[String, Machine] = em.flatMap(m => rule(m, input.map(i => if (i == "c") Coin else Turn).get))
      ((0, 0), machine)
    }
    })

  def errorInfoProgran(message: String): Free[Console, Unit] =
    printLn("Error: " + message)

  def currentStatusProgram(m: Machine): Free[Console, Unit] = m match {
    case Machine(locked, candies, coins) if locked => printLn(s"The machine is locked and has $candies candies left")
    case Machine(locked, candies, coins) => printLn(s"The machine is unlocked and has $candies candies left")
  }

  def candyDispencer(m: Machine): Machine = {
    runConsole(printLn(""))
    runConsole(currentStatusProgram(m))
    val result = runConsole(candyProgram()).run(Right(m))
    result._2 match {
      case Left(message) =>
        runConsole(errorInfoProgran(message))
        candyDispencer(m)
      case Right(m) =>
        candyDispencer(m)
    }
  }

  def main(args: Array[String]): Unit = {
    candyDispencer(Machine(true, 10, 0))

    //val res = runConsoleState(candyProgram()).run(Buffers(List("c"), List()))
    //res._2.out.reverse().forEach(a => println(a))
    //println(res._1.run(Machine(true, 10, 0)))
  }
}