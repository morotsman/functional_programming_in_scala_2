package chapter11

import chapter4.Option
import chapter8.{Gen, Prop, SGen}
import org.scalatest.FunSuite

class FunctorTest extends FunSuite {

  test("testDistribute") {

  }

  test("testCodistribute") {

  }

  test("testMap") {

  }

  test("Laws for list") {
    val law = ListFunctor.Laws.preserveStructure(ListFunctor)(Gen.listOf(Gen.int))
    Prop.run(law)
  }

  test("Laws for option") {
    val generator: Gen[Option[Int]] = Gen.chapter4OptionOf(Gen.int)
    val law = Functor.Laws.preserveStructure(OptionFunctor)(generator)
    Prop.run(law)
  }

}
