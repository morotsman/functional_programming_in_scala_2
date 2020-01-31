package chapter13

import org.scalatest.FunSuite

class FactorialTest extends FunSuite {


  test("factorial") {
    assert(Factorial.factorial2(0) == 1)
    assert(Factorial.factorial2(1) == 1)
    assert(Factorial.factorial2(2) == 2)
    assert(Factorial.factorial2(3) == 6)
    assert(Factorial.factorial2(4) == 24)
  }

}
