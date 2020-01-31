package chapter13


object FahrenheitConverter {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def ReadLine: IO[String] = IO {
    readLine
  }

  def PrintLine(message: String): IO[Unit] = IO {
    println(message)
  }

  def converter(): IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    - <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]): Unit = {
    println("start")
    converter.run
    println("stop")
  }


}
