package chapter13

import chapter13.IO1._

object FahrenheitConverter {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0


  def converter(): IO[Unit] = for {
    _ <- IO.PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- IO.ReadLine.map(_.toDouble)
    - <- IO.PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]): Unit = {
    converter.run
  }


}
