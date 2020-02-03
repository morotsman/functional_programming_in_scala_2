package chapter13

import chapter13.IO1._

object StackOverflow {

  def main(args: Array[String]): Unit = {
    val p = IO.forever(IO.PrintLine("Still going"))
    p.run
  }

}
