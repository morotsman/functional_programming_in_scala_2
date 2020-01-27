package chapter10

import chapter3.{Cons, List}
import chapter8.{Gen, Prop}
import org.scalatest.FunSuite

class FoldableTest extends FunSuite {

  test("testToList") {
    val prop = Prop.forAll(Gen.listOf(Gen.int))(l => {
      FoldableList.toList(l) == l
    })
    Prop.run(prop, testCases = 100)
  }

  test("testFoldLeft") {
    val prop = Prop.forAll(Gen.listOf(Gen.int))(l => {
      FoldableList.foldLeft(l)(List[Int]())((b, a) => Cons(a, b)) == l.reverse
    })
    Prop.run(prop, testCases = 100)

  }

  test("testFoldRight") {
    val prop = Prop.forAll(Gen.listOf(Gen.int))(l => {
      FoldableList.foldRight(l)(List[Int]())((a, b) => Cons(a, b)) == l
    })
    Prop.run(prop, testCases = 100)
  }

  test("testFoldMap") {
    val prop = Prop.forAll(Gen.listOf(Gen.choose(1, 100)))(l => {
      FoldableList.foldMap(l)(a => a)(Monoid.intAddition) == l.toScalaList().sum
    })
    Prop.run(prop, testCases = 100)
  }

  test("productMonoid usage") {

    val prop1 = Prop.forAll(Gen.listOf(Gen.choose(1, 10)))(l => {
      val productMonoid = Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication)
      FoldableList.foldMap(l)(a => (a, a))(productMonoid) == (l.toScalaList().sum, l.toScalaList().fold(1)(_ * _))
    })
    Prop.run(prop1, testCases = 100, maxSize = 10)

    val prop2 = Prop.forAll(Gen.listOf(Gen.choose(1, 10)))(l => {
      val productMonoid = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
      FoldableList.foldMap(l)(a => (1, a))(productMonoid) == (l.length, l.toScalaList().sum)
    })
    Prop.run(prop2, testCases = 100, maxSize = 10)
  }

}
