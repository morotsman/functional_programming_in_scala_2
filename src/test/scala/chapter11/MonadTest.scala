package chapter11

import chapter4.{None, Option, Some}
import chapter6.State
import chapter8.{Gen, Prop}
import org.scalatest.FunSuite


class MonadTest extends FunSuite {

  test("testParserMonad") {
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

  test("testListMonad") {
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


    sealed trait Input
    case class Number(n: Int) extends Input
    case class LotteryTicket(ns: List[Int], numberOfMatches: Int)

    def check: Input => LotteryTicket => LotteryTicket = (i: Input) => (l: LotteryTicket) => {
      (i, l) match {
        case (Number(n), LotteryTicket(ns, numberOfMatches)) if ns.contains(n) => LotteryTicket(ns, numberOfMatches + 1)
        case _ => l
      }
    }

    val input = List(Number(1), Number(4), Number(29), Number(5), Number(27), Number(21), Number(34))
    val inputSeq: List[State[LotteryTicket, (Number, Int)]] = input.map((i: Number) => State((s: LotteryTicket) => {
      val ticket = check(i)(s)
      ((i, ticket.numberOfMatches), ticket)
    }))

    val lotteryTicket = LotteryTicket(List(1, 6, 5, 23, 45, 27), 0)
    val lotteryMonad = Monad.stateMonad[LotteryTicket]

    assert(lotteryMonad.sequence(inputSeq).run(lotteryTicket)._2 == LotteryTicket(List(1, 6, 5, 23, 45, 27), 3))
    assert(lotteryMonad.sequence(inputSeq).run(lotteryTicket)._1 ==
      List((Number(1), 1), (Number(4), 1), (Number(29), 1), (Number(5), 2), (Number(27), 3), (Number(21), 3), (Number(34), 3)))

    val result = lotteryMonad.replicateM(input.length, State((s: LotteryTicket) => {
      val ticket = check(Number(1))(s)
      ((1, ticket.numberOfMatches), ticket)
    })).run(lotteryTicket)

    assert(result == (List((1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7)), LotteryTicket(List(1, 6, 5, 23, 45, 27), 7)))


    def getState[S]: State[S, S] = State(s => (s, s))

    def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

    def zipWithIndex[A](as: List[A]): List[(Int, A)] =
      as.foldLeft(stateM.unit(List[(Int, A)]()))((acc, a) => for {
        xs <- acc
        n <- acc.get
        _ <- acc.set(n + 1)
      } yield (n, a) :: xs).run(0)._1.reverse

    def zipWithIndex2[A](as: List[A]): List[(Int, A)] =
      as.foldLeft(stateM.unit(List[(Int, A)]()))((acc, a) => for {
        xs <- acc
        n <- getState
        _ <- setState(n + 1)
      } yield (n, a) :: xs).run(0)._1.reverse


    assert(zipWithIndex(List("a", "b", "c")) == List((0, "a"), (1, "b"), (2, "c")))
    assert(zipWithIndex2(List("a", "b", "c")) == List((0, "a"), (1, "b"), (2, "c")))
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

  test("ReaderMonad usage") {
    //translated from https://engineering.dollarshaveclub.com/the-reader-monad-example-motivation-542c54ccfaa8

    type HTML = String
    case class Context(email: String)

    def div(children: List[HTML]): HTML =
      "<div>" + children.mkString("") + "</div>"

    def h1(children: List[HTML]): HTML =
      "<h1>" + children.mkString("") + "</h1>"

    def p(children: List[HTML]): HTML =
      "<p>" + children.mkString("") + "</p>"

    def view(): Reader[Context, HTML] = for (
      p <- page()
    ) yield div(List(p))

    def page(): Reader[Context, HTML] = for (
      c <- content()
    ) yield div(List(
      topNav(),
      c
    ))

    def topNav(): HTML =
      h1(
        List("OurSite.com")
      )

    def content(): Reader[Context, HTML] = for (
      context <- Reader.ask;
      r <- right()
    ) yield div(List(
      h1(List("Custom content for " + context.email)),
      left(),
      r
    ))

    def left(): HTML =
      p(List("This is the left side"))

    def right(): Reader[Context, HTML] = for (
      a <- article()
    ) yield div(List(a))

    def article(): Reader[Context, HTML] = for (
      w <- widget2()
    ) yield div(List(
      p(List("This is an article")),
      w
    ))

    def widget(): Reader[Context, HTML] = for (
      context <- Reader.ask
    ) yield div(List(
      p(List("Hey " + context.email + ", we've got a great offer for you!"))
    ))

    // same as widget
    def widget2(): Reader[Context, HTML] = new Reader[Context, HTML](context =>
      div(List(
        p(List("Hey " + context.email + ", we've got a great offer for you!"))
      ))
    )

    // same as widget and widget 2
    def widget3(): Reader[Context, HTML] = Reader.ask.map(context =>
      div(List(
        p(List("Hey " + context.email + ", we've got a great offer for you!"))
      ))
    )

    val result = view().run(Context("leopold.niklas@gmail.com"))

    assert(result == "<div><div><h1>OurSite.com</h1><div><h1>Custom content for leopold.niklas@gmail.com</h1><p>This is the left side</p><div><div><p>This is an article</p><div><p>Hey leopold.niklas@gmail.com, we've got a great offer for you!</p></div></div></div></div></div></div>")
  }

}
