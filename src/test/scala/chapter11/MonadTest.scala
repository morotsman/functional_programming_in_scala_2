package chapter11

import chapter4.{None, Option, Some}
import chapter6.State
import chapter8.{Gen, Prop}
import org.scalatest.FunSuite


class MonadTest extends FunSuite {

  test("testListMonad") {
    val listMonad = Monad.listMonad

    assert(listMonad.unit(1) == List(1))

    assert(listMonad.replicateM(2, List(1, 2)) == List(List(1, 1), List(1, 2), List(2, 1), List(2, 2)))

    val greaterThenZero = (a: Int) => if (a > 0) listMonad.unit(a) else listMonad.unit(a).flatMap(_ => List())
    assert(listMonad.traverse(List(1, 2, 3))(greaterThenZero) == List(List(1, 2, 3)))
    assert(listMonad.traverse(List(1, 2, 3, 0))(greaterThenZero) == List())

  }

  test("testGenMonad") {

  }

  test("testStreamMonad") {

  }

  test("testOptionMonad") {
    val optionMonad = Monad.optionMonad
    val option: Option[Int] = Some(1)

    assert(optionMonad.replicateM(3, option) == Some(List(1, 1, 1)))

    val greaterThenZero = (a: Int) => if (a > 0) optionMonad.unit(a) else optionMonad.unit(a).flatMap(_ => None)
    assert(optionMonad.traverse(List(1, 2, 3))(greaterThenZero) == Some(List(1, 2, 3)))
    assert(optionMonad.traverse(List(1, 2, 0, 3))(greaterThenZero) == None)

    val isGreaterThenZero = (a: Int) => if (a > 0) optionMonad.unit(true) else optionMonad.unit(false)
    assert(optionMonad.filterM(List(1, 2, 0, 3))(a => isGreaterThenZero(a)) == Some(List(1, 2, 3)))


    val optionIntGen: Gen[Option[Int]] = Gen.chapter4OptionOf(Gen.int)

    val leftIdentity = Monad.Laws.leftIdentity(optionMonad)(optionIntGen)
    Prop.run(leftIdentity)

    val leftIdentityCompose = Monad.Laws.leftIdentityCompose(optionMonad)(Gen.int)
    Prop.run(leftIdentityCompose)

    val rightIdentity = Monad.Laws.rightIdentity(optionMonad)(Gen.int)
    Prop.run(rightIdentity)

    val associative = Monad.Laws.associative(optionMonad)(optionIntGen)
    Prop.run(associative)

    val associativeCompose = Monad.Laws.associativeCompose(optionMonad)(Gen.int)
    Prop.run(associativeCompose)


  }

  test("idMonad") {
    val idM = Monad.idMonad
    assert(Id(2).value == 2)
    assert(Id(2).flatMap(a => idM.unit(a * 2)).value == 4)
    assert(Id(2).flatMap(a => idM.unit(4 * a)).map(a => 4 * a).value == 32)
    assert(idM.replicateM(3, Id(2)) == Id(List(2, 2, 2)))
  }

  test("stateMonad") {
    val stateM = Monad.stateMonad[Int]

    assert(stateM.unit("a").run(1) == ("a", 1))

    assert(stateM.unit(1).run(2) == (1, 2))

    assert(stateM.unit("abc").flatMap(s => stateM.unit(s + s)).run(1) == ("abcabc", 1))
    assert(stateM.unit("abc").flatMap(a => State(s => (a + a, s - 1))).run(1) == ("abcabc", 0))
  }

  test("ReaderMonad") {
    case class Context(dep1: String, dep2: String)

    val readerMonad = Reader.ReaderMonad[Context]

    val context = Context("one", "two")

    val test = Reader((c: Context) => {
    })

    readerMonad.sequence(List(test, test)).run(context)

    val test2 = (c: Context) => (i: Int) => context.toString + " " + i
    val reader2 = Reader(test2)
    assert(reader2.run(context)(4) == "Context(one,two) 4")


    val test3 = (c: Context) => (i: Int) => "hepp " + i
    val reader3 = Reader(test3)

    val tmp = readerMonad.sequence(List(reader2, reader3)).run(context)
    assert(tmp(0)(10) == "Context(one,two) 10")
    assert(tmp(1)(12) == "hepp 12")
  }

}
