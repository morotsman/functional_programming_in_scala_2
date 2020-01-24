package chapter4

import org.scalatest._

class EitherSpec  extends FlatSpec with Matchers{


  "Right(2).map(_+1)" should "result in Right(3)" in {
    assert(Either.Try(4/2).map(_+1) == Right(3))
  }
  def isLeft[E,A](e: Either[E,A]): Boolean = e match{
    case Left(_) => true
    case _ => false
  }

  "Either.Try(4/0).map(_+1)" should "result in Left(java.lang.ArithmeticException)" in {
    assert(isLeft(Either.Try(4/0).map(_+1))  == true)
  }


  "Either.Try(4/2).flatMap(v => Either.Try(6/v))" should "result in Right(3)" in {
    assert(Either.Try(4/2).flatMap(v => Either.Try(6/v))  == Right(3))
  }

  "Either.Try(4/0).flatMap(v => Either.Try(6/v))" should "result in Left(java.lang.ArithmeticException)" in {
    assert(isLeft(Either.Try(4/0).flatMap(v => Either.Try(6/v)))  == true)
  }


  "Either.Try(4/0).orElse(Either.Try(4/1)).map(_+1)" should "result in Right(5)" in {
    assert(Either.Try(4/0).orElse(Either.Try(4/1)).map(_+1) == Right(5))
  }

  "Either.Try(4/0).orElse(Either.Try(4/0)).map(_+1)" should "result in Left(java.lang.ArithmeticException)" in {
    assert(isLeft(Either.Try(4/0).orElse(Either.Try(4/0)).map(_+1)) == true)
  }

  "Either.Try(4/2).map2(Either.Try(6/3))(_+_)" should "result in Right(4)" in {
    assert(Either.Try(4/2).map2(Either.Try(6/3))(_+_)  == Right(4))
  }

  "Either.Try(4/0).map2(Either.Try(6/3))(_+_)" should "result in Left(java.lang.ArithmeticException)" in {
    assert(isLeft(Either.Try(4/0).map2(Either.Try(6/3))(_+_))  == true)
  }

  "Either.Try(4/2).map2(Either.Try(6/0))(_+_)" should "result in Left(java.lang.ArithmeticException)" in {
    assert(isLeft(Either.Try(4/2).map2(Either.Try(6/0))(_+_))  == true)
  }

  "Either.sequence(List(Either.Try(4/2), Either.Try(8/2), Either.Try(10/2)))" should "result in Right(List(2,4,5))" in {
    assert(Either.sequence(List(Either.Try(4/2), Either.Try(8/2), Either.Try(10/2)))  == Right(List(2,4,5)))
  }

  "Either.sequence(List(Either.Try(4/2), Either.Try(8/2), Either.Try(10/2)))" should "result in Left(java.lang.ArithmeticException)" in {
    assert(isLeft(Either.sequence(List(Either.Try(4/0), Either.Try(8/2))))  == true)
  }

  "Either.traverse(List(1, 2, 5))" should "result in Right(List(2,4,5))" in {
    assert(Either.traverse(List(1, 2, 5))(v => Either.Try(10/v))  == Right(List(10,5,2)))
  }

  "Either.traverse(List(1, 0, 5))" should "result in Left(java.lang.ArithmeticException)" in {
    assert(isLeft(Either.traverse(List(1, 0, 5))(v => Either.Try(10/v))) == true)

    val failedAge = Partial.Test{
      1/0
    }

    val failedName = Partial.Test{
      val hepp: String = null
      hepp.concat("test")
    }

    val goodAge = Partial.Test{
      100/10
    }

    val goodName = Partial.Test{
      "Niklas"
    }

    println(failedAge)
    println(failedName)
    println(goodAge)

    println(failedAge.map2(failedName)((a,n) => n + a))
    println(goodAge.map2(failedName)((a,n) => n + a))
    println(goodAge.map2(goodName)((a,n) => Person(a,n)))

    val goodPerson = goodAge.map2(goodName)((a,n) => Person(a,n))

    goodPerson.forEach{ p =>
      println(p)
    }

    val badPerson = goodAge.map2(failedName)((a,n) => Person(a,n))

    badPerson.forEach{ p =>
      println(p)
    }

    val seq = Partial.sequence(List(failedAge, goodAge, failedName, goodName))
    println(seq)

    seq.forEach{ s =>
      println(s)
    }
  }

}

case class Person(val age: Int, val name: String)
