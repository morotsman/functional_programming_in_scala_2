package chapter13

import chapter13.Free.Console.{printLn, _}
import chapter13.Free._
import chapter3.List
import chapter4.{Either, Left, Right}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def main(args: Array[String]): Unit = {
    //candyMachine(Machine(true, 10, 0))

    val res = runConsoleState(candyProgram(Machine(true, 10, 0))).run(Buffers(List("c"), List()))
    println(res._1)
    res._2.out.reverse().forEach(a => println(a))

    val res2 = runConsoleState(candyProgram(Machine(false, 10, 1))).run(Buffers(List("c"), List()))
    println(res2._1)
    res2._2.out.reverse().forEach(a => println(a))

    val res3 = runConsoleState(candyProgram(Machine(false, 10, 1))).run(Buffers(List("t"), List()))
    println(res3._1)
    res3._2.out.reverse().forEach(a => println(a))
  }

  def candyMachine(machine: Machine): Unit = {
    runConsole(candyProgram(machine)) match {
      case Left(_) =>
        candyMachine(machine)
      case Right(newMachine) =>
        candyMachine(newMachine)
    }
  }

  def candyProgram(machine: Machine): Free[Console, Either[String, Machine]] = for {
    _ <- showCurrentStatusProgram(machine)
    - <- printLn("Input: c(coin) or t(turn)")
    input <- readLn
    eitherMachine <- freeMonad.unit(rule(machine, input.map(i => if (i == "c") Coin else Turn).get))
    - <- eitherMachine match {
      case Left(message) => freeMonad.sequence_(printLn("Error: " + message), printLn(""))
      case _ => printLn("")
    }
  } yield (eitherMachine)

  def showCurrentStatusProgram(m: Machine): Free[Console, Unit] = m match {
    case Machine(locked, candies, coins) if locked => printLn(s"The machine is locked and has $candies candies left")
    case Machine(locked, candies, coins) => printLn(s"The machine is unlocked and has $candies candies left")
  }


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
}
