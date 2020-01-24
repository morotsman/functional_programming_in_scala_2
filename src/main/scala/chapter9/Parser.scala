package chapter9

import chapter8.{Gen, Prop}
import chapter9.JSON._

import scala.util.matching.Regex

trait Parsers[ParserError, Parser[+ _]] {
  self =>

  implicit def operators[A](p: Parser[A]) =
    ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParserError, A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def withErrorMessage[A](p: Parser[A], errorMessage: String): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A, B](as: Parser[A])(f: A => Parser[B]): Parser[B]


  def product[A, B](p1s: Parser[A], p2s: => Parser[B]): Parser[(A, B)] =
    p1s.flatMap(p1 => p2s.map(p2 => (p1, p2)))

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    sequence(List.fill(n)(p))

  def map[A, B](as: Parser[A])(f: A => B): Parser[B] =
    flatMap(as)(a => succeed(f(a)))

  def map2[A, B, C](as: Parser[A], bs: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(as)(a => map(bs)(b => f(a, b)))

  def sequence[A](as: List[Parser[A]]): Parser[List[A]] =
    as.foldRight(succeed(List[A]()))((a, acc) => map2(a, acc)((aa, bb) => aa :: bb))

  def many[A](off: Parser[A]): Parser[List[A]] =
    map2(off, many(off))((a, b) => a :: b) or succeed(List())

  def number(off: Parser[Char]): Parser[Int] =
    map(many(off))(as => as.size)

  def numberAInTermsOfSlice: Parser[Int] =
    char('a').many().slice().map(_.length)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    p.flatMap(a => many(p))

  def map2InTermsOfProduct[A, B, C](as: Parser[A], bs: Parser[B])(f: (A, B) => C): Parser[C] =
    product(as, bs).map(a => f(a._1, a._2))

  def failed[A](): Parser[A]

  def digit: Parser[Int] =
    regex("[0-9]*".r).map(s => s.toInt)

  def skip[A, B](pa: Parser[A])(pb: Parser[B]): Parser[A] =
    pa.flatMap(a => pb.map(_ => a))

  case class ParserOps[A](p: Parser[A]) {
    def skip[B](pb: Parser[B]): Parser[A] = self.skip(p)(pb)

    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice[A](): Parser[String] = self.slice(p)

    def many(): Parser[List[A]] = self.many(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def many1(): Parser[List[A]] = self.many1(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def withErrorMessage(errorMessage: String): Parser[A] = self.withErrorMessage(p, errorMessage)
  }


  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => {
        val result = run(succeed("hepp"))(s)
        result == Right("hepp")
      })

    def productLaw1[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => {
        val result = run(succeed(s) ** succeed(s))(s)
        result == Right((s, s))
      })

    def productLaw2[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => {
        val result = run(failed() ** succeed(s))(s)
        result == Left
      })

    def productLaw3[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => {
        val result = run(succeed(s) ** failed())(s)
        result == Left
      })
  }

}

trait JSonParser {

  def jsonParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import JSonParser._
    import P._

    val spaces: Parser[String] = char(' ').many().slice()

    def character(character: Char): Parser[Char] =
      succeed().skip(spaces).flatMap(_ => char(character)).skip(spaces)

    val comma = character(',')
    val colon = character(':')
    val rightBracket = character('[')
    val leftBracket = character(']')
    val leftCurlyBracket = character('{')
    val rightCurlyBracket = character('}')

    val jNull = string("null").map(_ => JNull())
    val jBool = (string("false") or string("true")).map(b => JBool(b == "true"))
    val jNumber = regex(doubleMatcher).map(ds => JNumber(ds.toDouble))
    val jString = regex(stringMatcher).map(s => JString(removeQuotes(s)))

    def jArray: Parser[JArray] =
      rightBracket.flatMap(_ =>
        many(propertyValue.skip(comma or succeed())).map(l => JArray(l.toIndexedSeq))
      ).skip(leftBracket)

    def jProperty: Parser[JProperty] =
      jString.skip(colon).flatMap(name => {
        propertyValue.map(value => JProperty(name.get, value))
      }).skip(comma.or(succeed())).withErrorMessage("Could not parse jProperty")

    def jObject: Parser[JObject] =
      leftCurlyBracket.flatMap(_ =>
        many(jProperty).map(lp => JObject(lp.map(p => p.toPair()).toMap))
      ).skip(rightCurlyBracket)

    def propertyValue = jString or jBool or jNumber or jObject or jArray or jNull

    jObject
  }

  private def removeQuotes(s: String): String = s.drop(1).substring(0, s.length - 2)
}

object JSonParser {
  val doubleMatcher = "^\\-?\\d+\\.?\\d*[E]?\\-?\\d*".r
  val stringMatcher = "^\".*?\"".r

}

trait JSON

object JSON {

  case class JNull() extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JProperty(name: String, value: JSON) extends JSON {
    def toPair(): (String, JSON) = (name, value)
  }

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

}
