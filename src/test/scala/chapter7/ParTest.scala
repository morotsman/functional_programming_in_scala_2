package chapter7

import org.scalatest.FunSuite

class ParTest extends FunSuite {

  test("Par usage") {
    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1)
        Par.unit(ints.headOption getOrElse (0))
      else {
        val (l, r) =  ints.splitAt(ints.length / 2)
        Par.map2(sum(l), sum(r))(_+_)
      }
  }

}
