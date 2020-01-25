package chapter12

import org.scalatest.FunSuite
import chapter4.{Either, Left, Right}
import chapter8.{Gen, Prop}
import chapter3.{Cons, List, Nil}
import chapter4.Option

class ApplicativeTest extends FunSuite {

  val listMonad = new Monad2[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
      case Nil => Nil
      case Cons(x, xs) => f(x).append(flatMap(xs)(f))
    }

    override def unit[A](a: => A): List[A] = List(a)
  }

  test("testJoin") {
    assert(listMonad.join(listMonad.unit(listMonad.unit(1))) == listMonad.unit(1))
  }

  test("streamApplicative") {
    val result: Stream[List[Int]] = Applicative.streamApplicative.sequence(List(Stream.continually(1), Stream.continually(2)))
    assert(result.take(3).toList.toString == "List(Cons(1,Cons(2,Nil)), Cons(1,Cons(2,Nil)), Cons(1,Cons(2,Nil)))")
  }

  test("eitherMonad") {
    val F = Monad2.eitherMonad[Exception]

    def validateFirstName(name: String): Either[Exception, String] =
      if (name == null || name == "") Left(new RuntimeException("First name is missing")) else Right(name)

    def validateLastName(name: String): Either[Exception, String] =
      if (name == null || name == "") Left(new RuntimeException("Last name is missing")) else Right(name)

    def validateAge(age: Int): Either[Exception, Int] =
      if (age < 40) Left(new RuntimeException("To young")) else Right(age)


    val result: Either[Exception, (String, String, Int)] = for (
      firstName <- validateFirstName("Niklas");
      lastName <- validateLastName("Leopold");
      phone <- validateAge(42)
    ) yield (firstName, lastName, phone)

    assert(result == Right("Niklas", "Leopold", 42))


    val failedResult: Either[Exception, (String, String, Int)] = for (
      firstName <- validateFirstName("");
      lastName <- validateLastName("Leopold");
      phone <- validateAge(42)
    ) yield (firstName, lastName, phone)

    assert(failedResult.toString == Left(new RuntimeException("First name is missing")).toString)

    val firstName: Either[Exception, String] = validateFirstName("Niklas")
    val lastName: Either[Exception, String] = validateLastName("Leopold")
    val age: Either[Exception, Int] = validateAge(42)

    assert(F.map(firstName)(a => "test") == Right("test"))

    val map2Result = F.map2(firstName, lastName)((f, l) => f + " " + l)
    assert(map2Result == Right("Niklas Leopold"))


    val map3Result = F.map3(validateFirstName(""), lastName, age)((f, l, a) => f + " " + l + " is " + a + " years old.")
    assert(map3Result.toString == Left(new RuntimeException("First name is missing")).toString)

  }

  test("validationApplicative") {
    val F = Applicative.validationApplicative[Exception]

    def validateFirstName(name: String): Validation[Exception, String] =
      if (name == null || name == "") Failure(new RuntimeException("First name is missing")) else Success(name)

    def validateLastName(name: String): Validation[Exception, String] =
      if (name == null || name == "") Failure(new RuntimeException("Last name is missing")) else Success(name)

    def validateAge(age: Int): Validation[Exception, Int] =
      if (age < 40) Failure(new RuntimeException("To young")) else Success(age)

    def render(firstName: String, lastName: String, age: Int): String =
      firstName + " " + lastName + " " + age


    val result = F.map3(validateFirstName(""), validateLastName(""), validateAge(20))(render)

    assert(result.toString == "Failure(java.lang.RuntimeException: First name is missing,Vector(java.lang.RuntimeException: Last name is missing, java.lang.RuntimeException: To young))")

    val result2 = F.map3(validateFirstName("abc"), validateLastName("def"), validateAge(45))(render)

    assert(result2.toString == "Success(abc def 45)")
  }

  test("Applicative laws") {
    val generator: Gen[Option[Int]] = Gen.chapter4OptionOf(Gen.int)
    val law = Applicative.laws(Applicative.optionApplicative)(generator)
    Prop.run(law)
  }

  test("Traverse") {

    val tree: Tree[List[Int]] = Tree(List(1), List())

    val tmp: List[Tree[Int]] = Traverse.treeTraverse.sequence(tree)(List[Int]())

    assert(tmp == "")
  }

}
