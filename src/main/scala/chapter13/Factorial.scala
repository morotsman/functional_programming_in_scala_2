package chapter13

object Factorial {

  val helpstring = """
                     | The Amazing Factorial REPL, v2.0
                     | q - quit
                     | <number> - compute the factorial of the given number
                     | <anything else> - bomb with horrible error
  """.trim.stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- IO.ref(1)
    _ <- IO.foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = IO.sequence_(
    IO { println(helpstring) },
    IO.doWhile { IO { readLine } } { line =>
      val ok = line != "q"
      IO.when (ok) { for {
        n <- factorial(line.toInt)
        _ <- IO { println("factorial: " + n) }
      } yield () }
    }
  )

  def main(args: Array[String]): Unit = {
    factorialREPL.run
  }
}
