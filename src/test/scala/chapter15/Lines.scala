package chapter15

import java.util.concurrent.Executors

import chapter13.Free.{Free, IO, unsafePerformIO}
import chapter12.Monad2._
import chapter7.Par.Par
import org.scalatest.FunSuite

class Lines extends FunSuite{

  val es = Executors.newFixedThreadPool(1)

  def linesGt2(filename: String): IO[Boolean] = IO {
    // There are a number of convenience functions in scala.io.Source
    // for reading from external sources such as files.
    val src = io.Source.fromResource(filename)
    try {
      var count = 0
      // Obtain a stateful iterator from the Source
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next // has side effect of advancing to next element
        count += 1
      }
      count > 2
    }
    finally src.close
  }

  test("") {
    assert(unsafePerformIO(linesGt2("test.txt"))(es))
  }
}
