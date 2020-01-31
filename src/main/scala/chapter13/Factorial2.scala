package chapter13

import chapter13.FahrenheitConverter2.fahrenheitToCelsius
import chapter13.IO2._

object Factorial2 {

  val helpstring =
    """
      | The Amazing Factorial REPL, v2.0
      | q - quit
      | <number> - compute the factorial of the given number
      | <anything else> - bomb with horrible error
  """.trim.stripMargin


  def factorial2(n: Int): Int = {
    def go(i: Int, acc: Int): Int =
      if (i > n) acc
      else if (i == 0) go(i + 1, 1)
      else go(i + 1, acc * i)

    go(0, 1)
  }


  def factorialREPL2: IO[Unit] = IO.sequence_(
    PrintLine(helpstring),
    IO.doWhile { ReadLine } { line =>
      val ok = line != "q"
      IO.when(ok) {
        for {
          - <- PrintLine("factorial: "  + factorial2(line.toInt).toString)
        } yield ()
      }
    })

  def factorialREPL3: IO[Unit] =for {
    _ <- PrintLine(helpstring)
    _ <- IO.doWhile { ReadLine } { line =>
      val ok = line != "q"
      IO.when(ok) {
        for {
          - <- PrintLine("factorial: "  + factorial2(line.toInt).toString)
        } yield ()
      }
    }
  } yield ()

  def main(args: Array[String]): Unit = {
    run(factorialREPL2)
  }
}
