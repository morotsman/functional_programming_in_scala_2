package chapter13

import java.util.concurrent.Executors

import chapter13.IO4._
import chapter7.Par

object Factorial4 {

  val helpstring =
    """
      | The Amazing Factorial REPL, v2.0
      | q - quit
      | <number> - compute the factorial of the given number
      | <anything else> - bomb with horrible error
  """.trim.stripMargin


  def factorial(n: Int): Int = {
    def go(i: Int, acc: Int): Int =
      if (i > n) acc
      else if (i == 0) go(i + 1, 1)
      else go(i + 1, acc * i)

    go(0, 1)
  }


  def factorialREPL2: Async[Unit] = Async.sequence_(
    PrintLine(helpstring),
    Async.doWhile { ReadLine } { line =>
      val ok = line != "q"
      Async.when(ok) {
        for {
          - <- PrintLine("factorial: "  + factorial(line.toInt).toString)
        } yield ()
      }
    })

  def factorialREPL3: Async[Unit] =for {
    _ <- PrintLine(helpstring)
    _ <- Async.doWhile { ReadLine } { line =>
      val ok = line != "q"
      Async.when(ok) {
        PrintLine("factorial: "  + factorial(line.toInt).toString)
      }
    }
  } yield ()

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(1)
    Par.run(es)(run(factorialREPL2))
  }
}
