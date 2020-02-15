package chapter15

import org.scalatest.FunSuite

class ProcessTest extends FunSuite {

  test("lift") {
    val units = Stream.continually(())
    val ones = Process.lift((u: Unit) => 1)(units)
    assert(Stream(1, 1, 1) == ones.take(3))
  }

  test("filter") {
    val even = Process.filter((x: Int) => x % 2 == 0)
    assert(Stream(2, 4) == even(Stream(1, 2, 3, 4)))
  }

  test("sum") {
    assert(Stream(1.0, 3.0, 6.0, 10.0) == Process.sum(Stream(1.0, 2.0, 3.0, 4.0)))
  }

}
