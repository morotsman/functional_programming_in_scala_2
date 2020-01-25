package chapter10

import chapter6.{RNG, State}
import chapter8.{Gen, Prop}
import org.scalatest.FunSuite

import scala.util.Random

class MonoidTest extends FunSuite {

  test("testListMonoid") {
    val listGenarator = Gen.string.listOfN(Gen.choose(0, 10))
    Prop.run(Monoid.Laws.identityLaw(Monoid.listMonoid[String])(listGenarator))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.listMonoid[String])(listGenarator))
  }

  test("testEndoMonoid") {
    //Prop.run(Monoid.Laws.identityLaw(Monoid.endoMonoid[Int])(Gen.endoFunction))
    //Prop.run(Monoid.Laws.associativeLaw(Monoid.endoMonoid[Int])(Gen.endoFunction))
  }

  test("testIntAddition") {
    Prop.run(Monoid.Laws.identityLaw(Monoid.intAddition)(Gen.int))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.intAddition)(Gen.int))
  }

  test("testBooleanOr") {
    Prop.run(Monoid.Laws.identityLaw(Monoid.booleanOr)(Gen.boolean))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.booleanOr)(Gen.boolean))
  }

  test("testOptionMonoid") {
    val optionGenerator = Gen.chapter4OptionOf(Gen.int)
    Prop.run(Monoid.Laws.identityLaw(Monoid.optionMonoid[Int])(optionGenerator))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.optionMonoid[Int])(optionGenerator))

  }

  test("testStringMonoid") {
    Prop.run(Monoid.Laws.identityLaw(Monoid.stringMonoid)(Gen.string))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.stringMonoid)(Gen.string))
  }

  test("testBooleanAnd") {
    Prop.run(Monoid.Laws.identityLaw(Monoid.booleanAnd)(Gen.boolean))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.booleanAnd)(Gen.boolean))
  }

  test("testIntMultiplication") {
    Prop.run(Monoid.Laws.identityLaw(Monoid.intMultiplication)(Gen.int))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.intMultiplication)(Gen.int))
  }

  test("testOrderedMonoid") {
    Prop.run(Monoid.Laws.identityLaw(Monoid.orderedMonoid)(Gen.chapter4OptionOf(Gen.int)))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.orderedMonoid)(Gen.chapter4OptionOf(Gen.int)))
  }

  test("productMonoid") {
    Prop.run(Monoid.Laws.identityLaw(Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication))(Gen.pairOfInt))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication))(Gen.pairOfInt))
  }

  test("ordered") {
    val sortedProp = Prop.forAll(Gen.listOf(Gen.choose(-5,5)))(l => {
      Monoid.ordered(l.sorted) == true
    })
    Prop.run(sortedProp, maxSize = 20, testCases = 1000)


    val prop = Prop.forAll(Gen.listOf(Gen.choose(-5,5)))(l => {
      Monoid.ordered(l) == (l == l.sorted)
    })
    Prop.run(prop, maxSize = 20, testCases = 1000)
  }

  test("foldMapV") {
    val prop = Prop.forAll(Gen.listOf(Gen.choose(0,8)))(l => {
      Monoid.foldMapV(l.toIndexedSeq, Monoid.intAddition)(a => a) == l.sum
    })
    Prop.run(prop)
  }

  test("orderedV") {
    val sortedProp = Prop.forAll(Gen.listOf(Gen.choose(-5,5)))(l => {
      Monoid.orderedV(l.sorted.toIndexedSeq) == true
    })
    Prop.run(sortedProp, testCases = 1000, maxSize = 20)

    val prop = Prop.forAll(Gen.listOf(Gen.choose(-5,5)))(l => {
      val isSorted = l == l.sorted
      Monoid.orderedV(l.toIndexedSeq) == isSorted
    })
    Prop.run(prop, testCases = 1000, maxSize = 20)
  }

  val genWC: Gen[WC] = Gen.boolean.map(b => if (true) Stub("a") else Part("a", 1 , "b"))

  test("wcMonoid") {
    Prop.run(Monoid.Laws.identityLaw(Monoid.wcMonoid)(genWC))
    Prop.run(Monoid.Laws.associativeLaw(Monoid.wcMonoid)(genWC))
  }

  test("wordCount") {
    val sortedProp = Prop.forAll(Gen.sentence())(s => {
      if (s == "") {
        Monoid.wordCound(s) == 0
      } else {
        Monoid.wordCound(s) == s.trim.replaceAll(" +", " ").split(" ").size
      }
    })
    Prop.run(sortedProp, testCases = 1000, maxSize = 20)
  }

  test("bag") {
    val result = Monoid.bag(Vector("a", "rose", "is", "a", "rose"))
    assert(result == Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }

  /*
  test("breaking the law") {
    val intDiv = new Monoid[Double] {
      override def op(a1: Double, a2: Double): Double = a2 / a1

      override def zero: Double = 1
    }

    Prop.run(Monoid.Laws.identityLaw(intDiv)(Gen.double))
    Prop.run(Monoid.Laws.associativeLaw(intDiv)(Gen.double))
  }
  */

}
