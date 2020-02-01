package chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import Par._

class ParTest extends FunSuite {


  test("Par usage") {
    def sum(ints: IndexedSeq[Int]): Par[Int] = {
      println("sum: " + ints)
      Thread.sleep(1000)
      if (ints.size <= 1)
        Par.unit(ints.headOption getOrElse (0))
      else {
        val (l, r) =  ints.splitAt(ints.length / 2)
        Par.map2(fork(sum(l)), fork(sum(r)))(_+_)
      }
    }


    val es = Executors.newFixedThreadPool(100)
    assert(Par.run(es)(sum(List(1,2,3,4,5,6,7,8,9).toIndexedSeq)).get() == 45)
  }

}
