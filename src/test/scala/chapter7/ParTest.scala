package chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import Par._
import chapter3.List
import chapter8.{Gen, Prop}

class ParTest extends FunSuite {

  val es = Executors.newFixedThreadPool(100)


  test("Par usage") {
    def sum(ints: IndexedSeq[Int]): Par[Int] = {
      if (ints.size <= 1)
        Par.unit(ints.headOption getOrElse (0))
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(fork(sum(l)), fork(sum(r)))(_ + _)
      }
    }

    val prop = Prop.forAll(Gen.listOf(Gen.int)) { l =>
      Par.run(es)(sum(l.toScalaList().toIndexedSeq)).get() == l.toScalaList().sum
    }
    Prop.run(prop)
  }

  test("asyncF") {
    def add2(a: Int): Int = a + 2

    val asyncAdd2: Int => Par[Int] = asyncF(add2)
    assert(Par.run(es)(asyncAdd2(2)).get() == 4)
  }

  test("sequence") {
    val prop = Prop.forAll(Gen.listOf(Gen.int)) { l =>
      val seq = sequence(l.map(unit))
      Par.run(es)(seq).get() == l
    }
    Prop.run(prop)
  }
}
