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

  test("take") {
    val stream = Stream(1, 2, 3, 4)
    assert(stream == Process.take(6)(stream))
    assert(Stream(1, 2) == Process.take(2)(stream))
    assert(Stream() == Process.take(0)(stream))
  }

  test("drop") {
    val stream = Stream(1.0, 2.0, 3.0, 4.0)
    assert(stream == Process.drop(0)(stream))
    assert(Stream(3.0, 4.0) == Process.drop(2)(stream))
    assert(Stream() == Process.drop(6)(stream))

    assert(Stream(1.0, 3.0) == Process.take(2)(Process.sum(stream)))
  }

  test("takeWhile") {
    val stream = Stream(1, 2, 3, 4)
    assert(stream == Process.takeWhile((_: Int) => true)(stream))
    assert(Stream(1, 2) == Process.takeWhile((i: Int) => i < 3)(stream))
    assert(Stream() == Process.takeWhile((i: Int) => i < 0)(stream))
  }

  test("dropWhile") {
    val stream = Stream(1, 2, 3, 4)
    assert(stream == Process.dropWhile((_: Int) => false)(stream))
    assert(Stream(3, 4) == Process.dropWhile((i: Int) => i < 3)(stream))
    assert(Stream() == Process.dropWhile((i: Int) => i < 6)(stream))
  }

  test("count") {
    val stream = Stream("a", "b", "c", "d")
    assert(Stream(1, 2, 3, 4) == Process.count(stream))
    assert(Stream() == Process.count(Stream()))
  }

  test("mean") {
    val stream = Stream(1.0, 1.0, 2.0, 2.0)
    assert(Stream(1.0, 1.0, 1.3333333333333333, 1.5) == Process.mean(stream))
  }

  test("sumInTermsOfLoop") {
    assert(Stream(1.0, 3.0, 6.0, 10.0) == Process.sumInTermsOfLoop(Stream(1.0, 2.0, 3.0, 4.0)))
  }

  test("countInTermsOfLoop") {
    val stream = Stream("a", "b", "c", "d")
    assert(Stream(1, 2, 3, 4) == Process.countInTermsOfLoop(stream))
    assert(Stream() == Process.countInTermsOfLoop(Stream()))
  }

  test("forEach") {
    val stream = Stream("aa", "b", "cccc", "ddd")

    assert(stream == Process.forEach()(stream))
  }

  test("map") {
    val stream = Stream("aa", "b", "cccc", "ddd")

    val process = Process.forEach().map((i: String) => i.length)

    assert(Stream(2, 1, 4, 3) == process(stream))
  }
}
