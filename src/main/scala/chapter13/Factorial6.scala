package chapter13

import java.util.concurrent.{Executor, Executors}

import chapter3.List
import chapter13.Free._
import chapter13.Free.Console._
import chapter7.Par

object Factorial6 {

  val es = Executors.newFixedThreadPool(1)

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

  def factorialREPL: Free[Console, Unit] = for {
    _ <- printLn(helpstring)
    _ <- freeMonad.doWhile {
      readLn
    } { line =>
      freeMonad.when(line.get != "q") {
        printLn("factorial: " + factorial(line.get.toInt).toString)
      }
    }
  } yield ()

  def main(args: Array[String]): Unit = {
    //runConsole(factorialREPL)
    //Par.run(es)(runConsolePar(factorialREPL))
    //runConsoleReader(factorialREPL).run("q")
    val tmp: ConsoleState[Unit] = runConsoleState(factorialREPL)
    val res: (Unit, Buffers) = tmp.run(Buffers(List("1", "3", "6", "q"), List()))
    println(res._2.out)
  }
}
