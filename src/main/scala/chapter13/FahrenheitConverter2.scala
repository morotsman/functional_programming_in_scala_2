package chapter13

import chapter13.IO2._

object FahrenheitConverter2 {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0


  def converter(): IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    - <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]): Unit = {
    run(converter)
  }


}
