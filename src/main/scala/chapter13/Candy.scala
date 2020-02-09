package chapter13

import chapter13.Free.Console._
import chapter13.Free._
import chapter4.{Either, Left, Right}

import scala.annotation.tailrec

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def main(args: Array[String]): Unit =
    candyMachine(Machine(true, 10, 0))

  @tailrec
  def candyMachine(machine: Machine): Unit =
    candyMachine(runConsole(candyProgram(machine)).getOrElse(machine))

  def candyProgram(machine: Machine): Free[Console, Either[String, Machine]] = for {
    _ <- showCurrentStatus(machine)
    input <- getInput()
    newMachine <- freeMonad.unit(Rule.applyRule(machine, input))
    _ <- displayOutcome(newMachine)
  } yield (newMachine.map(m => m._2))

  def showCurrentStatus(m: Machine): Free[Console, Unit] = m match {
    case Machine(locked, candies, coins) if locked => printLn(s"The machine is locked and has $candies candies left")
    case Machine(locked, candies, coins) => printLn(s"The machine is unlocked and has $candies candies left")
  }

  def getInput(): Free[Console, Input] = for {
    - <- printLn("Input: c(coin) or t(turn)")
    input <- readLn
  } yield input.map(i => if (i == "c") Coin else Turn).get

  def displayOutcome(machine: Either[String, (String, Machine)]):Free[Console, Unit] = machine match {
    case Left(message) => freeMonad.sequence_(printLn("Error: " + message), printLn(""))
    case Right((message, m)) => freeMonad.sequence_(printLn("Success: " + message),printLn(""))
  }
}

object Rule {
  def applyRule(machine: Machine, input: Input): Either[String, (String, Machine)] = input match {
    case Coin =>
      if (machine.candies == 0) {
        Left("No candies Left")
      } else if (machine.locked) {
        val unlocked = false
        Right("Unlocked, turn to get your candy", Machine(unlocked, machine.candies, machine.coins + 1))
      } else {
        Left("Could not accept coin, turn to get a candy")
      }
    case Turn =>
      if (machine.candies == 0) {
        Left("No candies Left")
      } else if (!machine.locked) {
        val locked = true
        Right("Here is your candy", Machine(locked, machine.candies - 1, machine.coins))
      } else {
        Left("You need to dispose a coin to get a candy")
      }
  }

}
