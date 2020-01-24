package chapter2

import org.scalatest.FunSuite

class chapter2Test extends FunSuite {

  test("fib") {
    assert(0 == chapter2.fib(1))
    assert(1 == chapter2.fib(2))
    assert(1 == chapter2.fib(3))
    assert(2 == chapter2.fib(4))
    assert(3 == chapter2.fib(5))
    assert(5 == chapter2.fib(6))
  }

  test("isSorted") {
    assert(true == chapter2.isSorted[Int](List(), _ < _))
    assert(true == chapter2.isSorted[Int](List(1), _ < _))
    assert(true == chapter2.isSorted[Int](List(1,2), _ < _))
    assert(false == chapter2.isSorted[Int](List(2,1), _ < _))
    assert(true == chapter2.isSorted[Int](List(1,2,3), _ < _))
    assert(false == chapter2.isSorted[Int](List(1,3,2), _ < _))
  }

}
