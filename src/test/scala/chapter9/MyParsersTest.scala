package chapter9

import org.scalatest.{FlatSpec, Matchers}

class MyParsersTest extends FlatSpec with Matchers {

  "string" should "match different strings" in {
    val parser = MyParsers()
    val helloParser = parser.string("hello")

    assert(parser.run(helloParser)("hello") == Right("hello"))
    assert(parser.run(helloParser)("hi") == Left(ParserError("[hi] did not match string [hello], invalid character after 0")))
    assert(parser.run(helloParser)("hello hi") == Right("hello"))
  }

  "or" should "match the first parser or the next" in {
    val parser = MyParsers()
    val helloParser = parser.string("hello")
    val hiParser = parser.string("hi")
    val helloOrHiParser = parser.or(helloParser, hiParser)

    assert(parser.run(helloOrHiParser)("hello") == Right("hello"))
    assert(parser.run(helloOrHiParser)("hi") == Right("hi"))
    assert(parser.run(helloParser)("abc") == Left(ParserError("[abc] did not match string [hello], invalid character after 0")))
  }

  "flatMap" should "match the first parser and the apply function" in {
    val parser = MyParsers()
    val helloParser = parser.string("hello")
    val hiParser = parser.string("hi")
    val helloHiParser = parser.flatMap(helloParser)(a => parser.map(hiParser)(b => a + b))

    assert(parser.run(helloHiParser)("hellohi") == Right("hellohi"))
    assert(parser.run(helloHiParser)("hi") == Left(ParserError("[hi] did not match string [hello], invalid character after 0")))
    assert(parser.run(helloHiParser)("hellobye") == Left(ParserError("[bye] did not match string [hi], invalid character after 5")))
  }

  "map" should "match the first parser and the apply function" in {
    val parser = MyParsers()
    val helloParser = parser.string("hello")
    val helloToHiParser = parser.map(helloParser)(a => "hi")

    assert(parser.run(helloToHiParser)("hello") == Right("hi"))
    assert(parser.run(helloToHiParser)("hi") == Left(ParserError("[hi] did not match string [hello], invalid character after 0")))
  }

  "slice" should "return string" in {
    val parser = MyParsers()
    val a = parser.char('a')
    val b = parser.char('b')
    val p = parser.slice(parser.many(parser.or(a,b)))

    assert(parser.run(p)("aaba") == Right("aaba"))
  }
}
