package chapter9

import chapter8.{Gen, Prop}
import chapter9.JSON._
import org.scalatest.{FlatSpec, Matchers}

class JSonParserTest extends FlatSpec with Matchers {

  "number regex" should "not match a boolean" in {
    val prop: Prop = Prop.forAll(Gen.boolean)(b => {
      val result = JSonParser.doubleMatcher.findAllIn(b + "").toList
      result.isEmpty
    })
    Prop.run(prop)
  }

  "number regex" should "not match a string" in {
    val prop: Prop = Prop.forAll(Gen.string)(s => {
      val result = JSonParser.doubleMatcher.findAllIn(s).toList
      result.isEmpty
    })
    Prop.run(prop)
  }

  "number regex" should "not match a string even if it contains a number" in {
    val result = JSonParser.doubleMatcher.findAllIn("should not be matched" + 1).toList
    result.isEmpty
  }

  "number regex" should "match doubles" in {
    val prop: Prop = Prop.forAll(Gen.double)(d => {
      val result = JSonParser.doubleMatcher.findAllIn(d + "").toList
      d == result.head.toDouble
    })
    Prop.run(prop, testCases = 1000)
  }

  "number regex" should "match ints" in {
    val prop: Prop = Prop.forAll(Gen.int)(i => {
      val result = JSonParser.doubleMatcher.findAllIn(i + "").toList
      i == result.head.toInt
    })
    Prop.run(prop)
  }

  "string matcher" should "match strings" in {
    val prop: Prop = Prop.forAll(Gen.string)(s => {
      val stringToMatch = "\"" + s + "\""
      val result = JSonParser.stringMatcher.findAllIn(stringToMatch).toList
      stringToMatch == result.head
    })
    Prop.run(prop)
  }

  "string matcher" should "only match strings in the beginning of the inpur" in {
    val prop: Prop = Prop.forAll(Gen.string)(s => {
      val stringToMatch = "true\"should not be matched\""
      val result = JSonParser.stringMatcher.findAllIn(stringToMatch).toList
      List() == result
    })
    Prop.run(prop)
  }


  case class MyJsonParser() extends JSonParser
  "json parser" should "match an empty object" in {
    val jsonParser = MyJsonParser().jsonParser(MyParsers())

    val (res, state) = jsonParser.parseIt.run(ParserInput("{}", 0))
    assert(res == Right(JObject(Map())))
    assert(state == ParserInput("", 2))
  }

  "json parser" should "match an object with one string property" in {
    val jsonParser = MyJsonParser().jsonParser(MyParsers())

    val (res, state) = jsonParser.parseIt.run(ParserInput("""{"first":"first value"}""", 0))
    assert(res == Right(JObject(Map("first" -> JString("first value")))))
    assert(state == ParserInput("", 23))
  }

  "json parser" should "match an object with one boolean property" in {
    val jsonParser = MyJsonParser().jsonParser(MyParsers())

    val (res, state) = jsonParser.parseIt.run(ParserInput("""{"first":true}""", 0))
    assert(res == Right(JObject(Map("first" -> JBool(true)))))
    assert(state == ParserInput("", 14))
  }

  "json parser" should "match an object with one array property" in {
    val jsonParser = MyJsonParser().jsonParser(MyParsers())

    val (res, state) = jsonParser.parseIt.run(ParserInput(""" { "first" : [ 1 , 2 , 3 ] } """, 0))
    assert(res == Right(JObject(Map(
      "first" -> JArray(List(JNumber(1),JNumber(2),JNumber(3)).toIndexedSeq)
    ))))
    assert(state == ParserInput("", 29))
  }

  "json parser" should "match an object with both one boolean property and one string property" in {
    val jsonParser: MyParser[JSON] = MyJsonParser().jsonParser(MyParsers())

    val (res, state) = jsonParser.parseIt.run(ParserInput("""{"one":true,"two":"two value"}""", 0))
    assert(res == Right(JObject(Map(
      "one" -> JBool(true),
      "two" -> JString("two value")
    ))))
    assert(state == ParserInput("", 30))
  }

  "json parser" should "match an object with both one boolean, string and number property" in {
    val jsonParser: MyParser[JSON] = MyJsonParser().jsonParser(MyParsers())

    val (res, state) = jsonParser.parseIt.run(ParserInput(""" { "one" : true , "two" : "two value" , "three" : 12.34 }   """, 0))
    assert(res == Right(JObject(Map(
      "one" -> JBool(true),
      "two" -> JString("two value"),
      "three" -> JNumber(12.34))
    )))
    assert(state == ParserInput("", 60))
  }

  "json parser" should "match an object with both one boolean, string, number and a object property" in {
    val jsonParser: MyParser[JSON] = MyJsonParser().jsonParser(MyParsers())

    val (res, state) = jsonParser.parseIt.run(ParserInput("""{"one":true,"two":"two value","three":12.34,"four":{"five":42}}""", 0))
    assert(res == Right(JObject(Map(
      "one" -> JBool(true),
      "two" -> JString("two value"),
      "three" -> JNumber(12.34),
      "four" -> JObject(Map(
        "five" -> JNumber(42)
      ))
    ))))
    assert(state == ParserInput("", 63))
  }

  "json parser" should "report error" in {
    val jsonParser: MyParser[JSON] = MyJsonParser().jsonParser(MyParsers())

    val (res, state) = jsonParser.parseIt.run(ParserInput(""" { "one" : true , "two" : "two value" , "three" : #12.34 }   """, 0))
    assert(res == Left(ParserError("[\"three\" : #12.34 }   ] did not match string [}], invalid character after 40")))
    assert(state == ParserInput("\"three\" : #12.34 }   ", 40))
  }

}
