package chapter13

import chapter13.Free.Console._
import chapter13.Free._

import chapter3.List

object BMICalculator2 {

  val helpstring =
    """
      | The Amazing BMI Calculator
      | Calculates your BMI
      |
  """.trim.stripMargin

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

  def bmiPrompt(): Free[Console, Unit] = for {
    _ <- printLn("Please enter your weight: ")
    weight <- readLn
    _ <- printLn("Please enter your height (cm): ")
    height <- readLn
    _ <- printLn(createMessage(bmi(weight.get.toInt, height.get.toInt)))
  } yield ()

  def bmiProgram(): Free[Console, Unit] = for {
    - <- printLn(helpstring)
    _ <- freeMonad.doWhile {
      for {
        _ <- printLn("q to quit, any other key continue...")
        v <- readLn
      } yield v
    } { line =>
      freeMonad.when(line.get != "q") {
        bmiPrompt()
      }
    }
  } yield ()

  def main(args: Array[String]): Unit = {
    val program = bmiProgram()
    //runConsole(program)

    //val res: (Unit, Buffers) = runConsoleState(program).run(Buffers(List("", "70", "178", "q"), List()))
    //res._2.out.reverse().forEach(a => println(a))
  }
}
