package chapter9

import chapter6.State

import scala.util.matching.Regex

case class ParserError(error: String)

case class ParserInput(inputString: String, location: Int)

case class MyParser[+A](parseIt: State[ParserInput, Either[ParserError, A]])

case class MyParsers() extends Parsers[ParserError, MyParser] {
  override def run[A](p: MyParser[A])(input: String): Either[ParserError, A] =
    p.parseIt.run(ParserInput(input,0))._1

  override implicit def string(s: String): MyParser[String] = MyParser(parseIt = State(input => {
    if (input.inputString.startsWith(s)) {
      (Right(s), ParserInput(input.inputString.drop(s.length), input.location + s.length))
    } else {
      (Left(ParserError("[" + input.inputString + "]" + " did not match string " + "[" + s + "], invalid character after " + input.location)), input)
    }
  }))

  def succeed[A](a: A): MyParser[A] = MyParser(parseIt = State(input => {
    (Right(a), input)
  }))

  override implicit def regex(r: Regex): MyParser[String] = MyParser(parseIt = State(input => {
    r.findFirstIn(input.inputString)
      .map(res => {
        (Right(res), ParserInput(input.inputString.drop(res.length), input.location + res.length))}
      )
      .getOrElse((Left(ParserError("[" + input + "]" + " did not match string " + "[" + r.toString() + "], invalid character after " + input.location)), input))
  }))

  override def slice[A](p: MyParser[A]): MyParser[String] = MyParser(parseIt = State(input => {
    val (res, input2) = p.parseIt.run(input)
    res match {
      case Right(v) => (Right(input.inputString.substring(0, input.inputString.length - input2.inputString.length)), input2)
      case Left(ParserError(e)) => (Left(ParserError(e)), input)
    }
  }))

  override def or[A](s1: MyParser[A], s2: => MyParser[A]): MyParser[A] = MyParser(parseIt = State(input => {
    val (res, input2: ParserInput) = s1.parseIt.run(input)
    res match {
      case Right(a) => (res, input2)
      case _ => s2.parseIt.run(input)
    }
  }))

  override def flatMap[A, B](as: MyParser[A])(f: A => MyParser[B]): MyParser[B] = MyParser(parseIt = State(input => {
    val (res, input2: ParserInput) = as.parseIt.run(input)
    res match {
      case Right(a) => f(a).parseIt.run(input2)
      case Left(ParserError(e)) => (Left(ParserError(e)), input)
    }
  }))

  override def failed[A](): MyParser[A] = MyParser(parseIt = State(input => {
    (Left(ParserError("Parsing failed")), input)
  }))

  override implicit def withErrorMessage[A](p: MyParser[A], errorMessage: String): MyParser[A] = MyParser(parseIt = State(input => {
    val (res, input2: ParserInput) = p.parseIt.run(input)
    res match {
      case Right(a) => (res, input2)
      case Left(ParserError(e)) => (Left(ParserError(errorMessage)), input)
    }
  }))
}

object MyParser {
  def unit[A](a: A): MyParser[A] = MyParser(parseIt = State(input => {
    (Right(a), input)
  }))
}
