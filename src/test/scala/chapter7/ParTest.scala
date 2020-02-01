package chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import Par._

class ParTest extends FunSuite {


  test("Par usage") {
    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1)
        Par.unit(ints.headOption getOrElse (0))
      else {
        val (l, r) =  ints.splitAt(ints.length / 2)
        Par.map2(sum(l), sum(r))(_+_)
      }
    val es = Executors.newFixedThreadPool(2)
    assert(Par.run(es)(sum(List(1,2,3).toIndexedSeq)).get() == 6)
  }

}
