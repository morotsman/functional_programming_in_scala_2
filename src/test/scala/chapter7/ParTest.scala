package chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import Par._
import chapter3.List
import chapter4.{Option, Some, None}
import chapter8.{Gen, Prop}

class ParTest extends FunSuite {

  val es = Executors.newFixedThreadPool(100)

  test("Par usage") {
    def sum(ints: IndexedSeq[Int]): Par[Option[Int]] = {
      if (ints.size <= 1)
        Par.unit(if (ints.isEmpty) None else Some(ints.head))
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(fork(sum(l)), fork(sum(r)))((ol, or) => Option.map2(ol, or)(_ + _))
      }
    }

    def max(ints: IndexedSeq[Int]): Par[Option[Int]] = {
      if (ints.size <= 1)
        Par.unit(if (ints.isEmpty) None else Some(ints.head))
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(fork(max(l)), fork(max(r)))((ol, or) => Option.map2(ol, or)(_ max _))
      }
    }

    val sumProp = Prop.forAll(Gen.listOf(Gen.int)) { l =>
      if (l.size() == 0)
        Par.run(es)(sum(l.toScalaList().toIndexedSeq)) == None
      else
        Par.run(es)(sum(l.toScalaList().toIndexedSeq)) == Some(l.toScalaList().sum)
    }

    val maxProp = Prop.forAll(Gen.listOf(Gen.int)) { l =>
      if (l.size() == 0)
        Par.run(es)(max(l.toScalaList().toIndexedSeq)) == None
      else
        Par.run(es)(max(l.toScalaList().toIndexedSeq)) == Some(l.toScalaList().max)
    }

    Prop.run(sumProp && maxProp)
  }

  test("fold usage") {
    def sum(ints: IndexedSeq[Int]): Par[Int] =
      parFold(ints)(0)(Par.unit(_))(_ + _)

    def max(ints: IndexedSeq[Int]): Par[Option[Int]] =
      parFold(ints)(None: Option[Int])(a => Par.unit(Some(a)))((a, b) => Option.map2(a, b)(_ max _))

    val sumProp = Prop.forAll(Gen.listOf(Gen.int)) { l =>
      if (l.size() == 0)
        Par.run(es)(sum(l.toScalaList().toIndexedSeq)) == 0
      else
        Par.run(es)(sum(l.toScalaList().toIndexedSeq)) == l.toScalaList().sum
    }

    val maxProp = Prop.forAll(Gen.listOf(Gen.int)) { l =>
      if (l.size() == 0)
        Par.run(es)(max(l.toScalaList().toIndexedSeq)) == None
      else
        Par.run(es)(max(l.toScalaList().toIndexedSeq)) == Some(l.toScalaList().max)
    }

    Prop.run(maxProp && sumProp)
  }


  test("asyncF") {
    def add2(a: Int): Int = a + 2

    val asyncAdd2: Int => Par[Int] = asyncF(add2)
    assert(Par.run(es)(asyncAdd2(2)) == 4)
  }

  test("sequence") {
    val prop = Prop.forAll(Gen.listOf(Gen.int)) { l =>
      val seq = sequence(l.map(unit))
      Par.run(es)(seq) == l
    }
    Prop.run(prop)
  }

  test(" parMap") {
    val prop = Prop.forAll(Gen.listOf(Gen.int)) { l =>
      val result = parMap(l)(a => a)
      Par.run(es)(result) == l.map(a => a)
    }
    Prop.run(prop)
  }

  test(" parFilter") {
    val prop = Prop.forAll(Gen.listOf(Gen.choose(-10, 10))) { l =>
      val predicate: Int => Boolean = a => a < 5
      val result = parFilter(l)(predicate)
      Par.run(es)(result) == l.filter(predicate)
    }
    Prop.run(prop)
  }

  test("countWords") {
    def wc(ps: List[List[String]]): Par[Int] = {
      val seq = ps.toScalaList().toIndexedSeq
      parFold(seq)(0)(a => Par.unit(a.size()))(_ + _)
    }

    assert(Par.run(es)(wc(List())) == 0)
    assert(Par.run(es)(wc(List(List()))) == 0)
    assert(Par.run(es)(wc(List(List("a", "b")))) == 2)
    assert(Par.run(es)(wc(List(List("a", "b"), List("a", "b", "c", "d")))) == 6)

  }
}
