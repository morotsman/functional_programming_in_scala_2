package chapter13

import chapter13.Free.Buffers
import org.scalatest.FunSuite
import chapter4.{Left, Right}
import chapter3.{List}
import chapter13.Free._

class CandyTest extends FunSuite {

  test("testDisplayOutcome") {
    val res1 = runConsoleState(Candy.displayOutcome(Left("Something went wrong"))).run(Buffers(List(), List()))
    assert(res1._1 == ())
    assert(res1._2.out.reverse() == List("Error: Something went wrong", ""))

    val res2 = runConsoleState(Candy.displayOutcome(Right("You got a candy", Machine(true, 10, 1)))).run(Buffers(List(), List()))
    assert(res2._1 == ())
    assert(res2._2.out.reverse() == List("Success: You got a candy", ""))
  }

  test("testCandyProgram") {
    val locked = true
    val open = false

    val res1 = runConsoleState(Candy.candyProgram(Machine(locked, 10, 0))).run(Buffers(List("c"), List()))
    assert(res1._1 == Right(Machine(open,10,1)))
    assert(res1._2.out.reverse() == List(
      "The machine is locked and has 10 candies left",
      "Input: c(coin) or t(turn)",
      "Success: Unlocked, turn to get your candy",
      ""
    ))

    val res2 = runConsoleState(Candy.candyProgram(Machine(open, 10, 1))).run(Buffers(List("t"), List()))
    assert(res2._1 == Right(Machine(locked,9,1)))
    assert(res2._2.out.reverse() == List(
      "The machine is unlocked and has 10 candies left",
      "Input: c(coin) or t(turn)",
      "Success: Here is your candy",
      ""
    ))

    val res3 = runConsoleState(Candy.candyProgram(Machine(locked, 10, 0))).run(Buffers(List("t"), List()))
    assert(res3._1 == Left("You need to dispose a coin to get a candy"))
    assert(res3._2.out.reverse() == List(
      "The machine is locked and has 10 candies left",
      "Input: c(coin) or t(turn)",
      "Error: You need to dispose a coin to get a candy",
      ""
    ))

    val res4 = runConsoleState(Candy.candyProgram(Machine(open, 10, 0))).run(Buffers(List("c"), List()))
    assert(res4._1 == Left("Could not accept coin, turn to get a candy"))
    assert(res4._2.out.reverse() == List(
      "The machine is unlocked and has 10 candies left",
      "Input: c(coin) or t(turn)",
      "Error: Could not accept coin, turn to get a candy",
      ""
    ))
  }

  test("testShowCurrentStatus") {
    val res1 = runConsoleState(Candy.showCurrentStatus(Machine(true, 10, 0))).run(Buffers(List(), List()))
    assert(res1._1 == ())
    assert(res1._2.out == List("The machine is locked and has 10 candies left"))

    val res2 = runConsoleState(Candy.showCurrentStatus(Machine(false, 10, 0))).run(Buffers(List(), List()))
    assert(res2._1 == ())
    assert(res2._2.out == List("The machine is unlocked and has 10 candies left"))
  }

  test("input t") {
    val res = runConsoleState(Candy.getInput()).run(Buffers(List("t"), List()))
    assert(res._1 == Turn)
    assert(res._2.out == List("Input: c(coin) or t(turn)"))
  }

  test("input c") {
    val res = runConsoleState(Candy.getInput()).run(Buffers(List("c"), List()))
    assert(res._1 == Coin)
    assert(res._2.out == List("Input: c(coin) or t(turn)"))
  }

  test("applyRule") {
    val locked = true
    val open = false
    assert(Rule.applyRule(Machine(locked, 10, 0), Coin) == Right(("Unlocked, turn to get your candy", Machine(open, 10, 1))))
    assert(Rule.applyRule(Machine(open, 10, 1), Turn) == Right(("Here is your candy", Machine(locked, 9, 1))))

    assert(Rule.applyRule(Machine(open, 10, 0), Coin) == Left("Could not accept coin, turn to get a candy"))
    assert(Rule.applyRule(Machine(locked, 10, 0), Turn) == Left("You need to dispose a coin to get a candy"))

    assert(Rule.applyRule(Machine(open, 0, 10), Coin) == Left("No candies Left"))
    assert(Rule.applyRule(Machine(locked, 0, 10), Turn) == Left("No candies Left"))
  }

}
