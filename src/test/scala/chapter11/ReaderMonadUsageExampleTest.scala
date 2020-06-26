package chapter11

import chapter4.{None, Option, Some}
import chapter6.State
import chapter8.{Gen, Prop}
import org.scalatest.FunSuite


class ReaderMonadUsageExampleTest extends FunSuite {

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
      w <- widget()
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
