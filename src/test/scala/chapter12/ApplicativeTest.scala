package chapter12

import org.scalatest.FunSuite
import chapter4.{Either, Left, Right}
import chapter8.{Gen, Prop}
import chapter3.{Cons, List, Nil}
import chapter4.{Option, Some, None}

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

  test("eitherMonad usage") {
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

  test("validationApplicative usage") {
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

  test("applicative compose") {
    val optionList = Applicative.optionApplicative.compose(Applicative.listApplicative)
    val result = optionList.map(Some(List(1, 2, 3)))(a => a + 5)
    assert(result == Some(List(6)))
  }

  test("applicative product") {
    val optionList = Applicative.optionApplicative.product(Applicative.listApplicative)
    val result = optionList.map((Some(1), List(2, 3, 4)))(a => a + 6)
    assert(result == (Some(7), List(8)))
  }

  test("Sequence tree of lists") {
    val smallTree: Tree[List[Int]] = Tree(List(1), List())
    val tallTree: Tree[List[Int]] = Tree(List(1), List(Tree(List(2, 1), List(Tree(List(3, 2, 1), List())))))
    val wideTree: Tree[List[Int]] = Tree(List(1), List(Tree(List(2, 1), List()), Tree(List(3, 2, 1), List())))

    val result1: List[Tree[Int]] = Traverse.treeTraverse.sequence(smallTree)
    assert(result1 == List(Tree(1, List())))

    val result2: List[Tree[Int]] = Traverse.treeTraverse.sequence(tallTree)
    assert(result2 == List(Tree(1, List(Tree(2, List(Tree(3, List())))))))

    val result3: List[Tree[Int]] = Traverse.treeTraverse.sequence(wideTree)
    assert(result3 == List(Tree(1, Cons(Tree(2, List()), List(Tree(3, List()))))))
  }

  test("Sequence tree of option") {
    val smallTree: Tree[Option[Int]] = Tree(Some(1), List())
    val tallTree: Tree[Option[Int]] = Tree(Some(1), List(Tree(Some(2), List(Tree(Some(3), List())))))
    val wideTree: Tree[Option[Int]] = Tree(Some(1), List(Tree(Some(2), List()), Tree(Some(3), List())))
    val treeWithNone: Tree[Option[Int]] = Tree(Some(1), List(Tree(Some(2), List()), Tree(None, List())))

    val result1: Option[Tree[Int]] = Traverse.treeTraverse.sequence(smallTree)
    assert(result1 == Some(Tree(1, List())))

    val result2: Option[Tree[Int]] = Traverse.treeTraverse.sequence(tallTree)
    assert(result2 == Some(Tree(1, List(Tree(2, List(Tree(3, List())))))))

    val result3: Option[Tree[Int]] = Traverse.treeTraverse.sequence(wideTree)
    assert(result3 == Some(Tree(1, Cons(Tree(2, List()), List(Tree(3, List()))))))

    val result4: Option[Tree[Int]] = Traverse.treeTraverse.sequence(treeWithNone)
    assert(result4 == None)
  }

  test("Traverse list of option") {
    val T = Traverse.listTraverse

    val result1: Option[List[Int]] = T.traverse(List(1, 2, 3))(a => Some(a): Option[Int])(Applicative.optionApplicative)
    assert(result1 == Some(List(1, 2, 3)))

    val result2: Option[List[Int]] = T.traverse(List(1, 2, -1, 3))(a => if (a >= 0) Some(a) else None)
    assert(result2 == None)
  }

  test("Traverse list of validation") {
    val T = Traverse.listTraverse

    def positve(value: Int): Validation[Exception, Int] =
      if (value >= 0) Success(value) else Failure(new RuntimeException(value + " is less then zero"))

    type VE[A] = Validation[Exception, A]
    val result1 = T.traverse(List(1, 2, 3))(a => positve(a): VE[Int])(Applicative.validationApplicative())
    assert(result1 == Success(List(1, 2, 3)))

    val result2 = T.traverse(List(1, -2, -3))(a => positve(a): VE[Int])(Applicative.validationApplicative())
    assert(result2.toString == "Failure(java.lang.RuntimeException: -2 is less then zero,Vector(java.lang.RuntimeException: -3 is less then zero))")
  }
}
