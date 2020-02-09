package chapter13

import chapter13.Free.Console._
import chapter13.Free._
import chapter3.List

import scala.annotation.tailrec

object BMICalculator2 {

  val helpstring: String =
    """
      | The Amazing BMI Calculator
      | Calculates your BMI
      |
  """.trim.stripMargin

  def main(args: Array[String]): Unit =
    runConsole(bmiProgram())

  def bmiProgram(): Free[Console, Unit] = for {
    - <- printLn(helpstring)
    _ <- freeMonad.doWhile(shouldContinue)(line => freeMonad.when(line.get != "q") {
      bmiPrompt()
    })
  } yield ()

  def shouldContinue: Free[Console, Option[String]] =
    for {
      _ <- printLn("q to quit, any other key continue...")
      v <- readLn
    } yield v

  def bmiPrompt(): Free[Console, Unit] = for {
    weight <- getIntInput("Please enter your weight: ")
    height <- getIntInput("Please enter your height (cm): ")
    _ <- printLn(createMessage(bmi(weight, height)))
  } yield ()

  def getIntInput(message: String): Free[Console, Int] = {
    val getInput = for {
      _ <- printLn(message)
      oi <- readLn
    } yield oi

    def isNumeric(input: String): Boolean =
      if (input == "") false else input.forall(_.isDigit)

    getInput.flatMap(oi =>
      if (oi.exists(isNumeric)) {
        freeMonad.unit(oi.get.toInt)
      } else {
        getIntInput(message)
      })
  }

  def bmi(weight: Int, heightInCm: Int): Double = {
    val heightInMeters = heightInCm.toDouble / 100
    weight / (heightInMeters * heightInMeters)
  }

  def createMessage(bmi: Double): String =
    if (bmi < 18.5) {
      s"Sorry, your underweight, your bmi is: $bmi"
    } else if (bmi >= 18.5 && bmi < 25) {
      s"Your have normal weight and your bmi is: $bmi"
    } else {
      s"Your overweight, your bmi is: $bmi"
    }
}
