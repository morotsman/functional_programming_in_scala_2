package chapter13

import chapter13.IO1._

object Factorial {

  val helpstring =
    """
      | The Amazing Factorial REPL, v2.0
      | q - quit
      | <number> - compute the factorial of the given number
      | <anything else> - bomb with horrible error
  """.trim.stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- IO.ref(1)
    _ <- IO.foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = IO.sequence_(
    IO { println(helpstring) },
    IO.doWhile { IO { readLine } } { line =>
      val ok = line != "q"
      IO.when(ok) {
        for {
          n <- factorial(line.toInt)
          _ <- IO {
            println("factorial: " + n)
          }
        } yield ()
      }
    }
  )

  def factorial2(n: Int): Int = {
    def go(i: Int, acc: Int): Int =
      if (i > n) acc
      else if (i == 0) go(i + 1, 1)
      else go(i + 1, acc * i)

    go(0, 1)
  }

  val factorialREPL2: IO[Unit] = IO.sequence_(
    IO { println(helpstring) },
    IO.doWhile { IO { readLine } } { line =>
      val ok = line != "q"
      IO.when(ok) {
        for {
          - <- IO.PrintLine("factorial: "  + factorial2(line.toInt).toString)
        } yield ()
      }
    })

  def main(args: Array[String]): Unit = {
    factorialREPL2.run
  }
}
