package chapter12

import chapter10.Monoid
import org.scalatest.FunSuite
import chapter4.{Either, Left, Right}
import chapter8.{Gen, Prop}
import chapter3.{Cons, List, Nil}
import chapter4.{None, Option, Some}
import chapter5.Stream

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

  test("streamApplicative unit") {
    assert(Applicative.streamApplicative.unit(1).take(3).toList == Stream(1, 1, 1).toList)
  }

  test("streamApplicative map2") {
    val s1 = Applicative.streamApplicative.unit(1)
    val s2 = Applicative.streamApplicative.unit(2)
    val A = Applicative.streamApplicative
    assert(A.map2(s1, s2)((a, b) => (a, b)).take(3).toList == Stream((1, 2), (1, 2), (1, 2)).toList)
  }

  test("listApplicative unit") {
    val A = Applicative.listApplicative
    assert(A.unit(1) == List(1))
  }

  test("listApplicative map2") {
    val A = Applicative.listApplicative
    assert(A.map2(List(1, 2), List(2, 3))((a, b) => (a, b)) == List((1, 2), (2, 3)))
    assert(A.map2(List(1, 2), List(2))((a, b) => (a, b)) == List((1, 2)))
    assert(A.map2(List(1), List(2, 3))((a, b) => (a, b)) == List((1, 2)))
    assert(A.map2(List(), List(2, 3))((a, b) => (a, b)) == List())
  }

  test("eitherApplicative unit") {
    val A = Applicative.eitherApplicative[Exception]
    assert(A.unit(1) == Right(1))
  }

  test("eitherApplicative map2") {
    val A = Applicative.eitherApplicative[String]
    assert(A.map2(Right(1), Right(2))((a, b) => (a, b)) == Right((1, 2)))
    assert(A.map2(Left("one"), Right(2))((a, b) => (a, b)) == Left("one"))
    assert(A.map2(Left("one"), Left("two"))((a, b) => (a, b)) == Left("one"))
    assert(A.map2(Right(1), Left("two"))((a, b) => (a, b)) == Left("two"))
  }

  test("optionApplicative unit") {
    val A = Applicative.optionApplicative
    assert(A.unit(1) == Some(1))
  }

  test("optionApplicative map2") {
    val A = Applicative.optionApplicative
    assert(A.map2(Some(1), Some(2))((a, b) => (a, b)) == Some((1, 2)))
    assert(A.map2(Some(1), None)((a, b) => (a, b)) == None)
    assert(A.map2(None, None)((a, b) => (a, b)) == None)
    assert(A.map2(None, Some(2))((a, b) => (a, b)) == None)
  }

  test("validationApplicative unit") {
    val A = Applicative.validationApplicative[String]()
    assert(A.unit(1) == Success(1))
  }

  test("validationApplicative map2") {
    val A = Applicative.validationApplicative[String]()
    assert(A.map2(Success(1), Success(2))((a, b) => (a, b)) == Success((1, 2)))
    assert((A.map2(Failure("one"), Success(2))((a, b) => (a, b))).toString == Failure("one").toString)
    assert((A.map2(Success(1), Failure("two"))((a, b) => (a, b))).toString == Failure("two").toString)
    assert((A.map2(Failure("one"), Failure("two"))((a, b) => (a, b))).toString == Failure("one", Vector("two")).toString)
  }

  test("option") {
    val A = Applicative.optionApplicative
    assert(A.map(A.unit(1))(a => a) == A.unit(1))
  }

  test("traverse") {
    val A = Applicative.optionApplicative
    assert(A.traverse(List(1, 2))(a => Some(a)) == Some(List(1, 2)))
    assert(A.traverse(List(1, 2))(a => None) == None)
    assert(A.traverse(List())(_ => None) == Some(List()))
  }

  test("sequence") {
    val A = Applicative.optionApplicative
    assert(A.sequence(List(Some(1))) == Some(List(1)))
    assert(A.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(A.sequence(List(Some(1), Some(2), None)) == None)
  }

  test("sequenceMap") {
    val A = Applicative.optionApplicative
    assert(A.sequenceMap(Map(1 -> Some(1))) == Some(Map(1 -> 1)))
    assert(A.sequenceMap(Map(1 -> None)) == None)
  }

  test("replicateM") {
    val A = Applicative.optionApplicative
    assert(A.replicateM(2, Some(2)) == Some(List(2, 2)))
  }

  test("product") {
    val A = Applicative.optionApplicative
    assert(A.product(Some(1), Some(2)) == Some((1, 2)))
    assert(A.product(Some(1), None) == None)
    assert(A.product(None, Some(2)) == None)
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


    val map3Result = F.map3(validateFirstName(""), validateLastName(""), age)((f, l, a) => f + " " + l + " is " + a + " years old.")
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

  test("apply") {
    val A = Applicative.optionApplicative
    assert(A.apply(Some[Int => Int](a => a))(Some(1)) == Some(1))
  }

  test("map3") {
    val A = Applicative.optionApplicative
    assert(A.map3(Some(1), Some(2), Some(3))((a, b, c) => (1, 2, 3)) == Some(1, 2, 3))
    assert(A.map3(Some(1), Some(2), None)((a, b, c) => (1, 2, 3)) == None)
  }

  test("map4") {
    val A = Applicative.optionApplicative
    assert(A.map4(Some(1), Some(2), Some(3), Some(4))((a, b, c, d) => (1, 2, 3, 4)) == Some(1, 2, 3, 4))
    assert(A.map4(Some(1), Some(2), None, Some(4))((a, b, c, d) => (1, 2, 3, 4)) == None)
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

  test("idApplicative unit") {
    val A = Applicative.idApplicative
    assert(A.unit(1).run() == 1)
  }

  test("idApplicative map2") {
    val A = Applicative.idApplicative
    assert(A.map2(Id(() => 1), Id(() => 2))((a, b) => (a, b)).run() == (1, 2))
  }

  test("traverse map") {
    val T = Traverse.listTraverse
    assert(T.map(List(1, 2, 3))(a => a) == List(1, 2, 3))
  }

  test("zipWithIndex2") {
    val T = Traverse.listTraverse
    val list = List("a", "b", "c")
    val expected = List(("a", 0), ("b", 1), ("c", 2))
    assert(T.zipWthIndex2(list) == expected)
  }

  test("zipWithIndex") {
    val T = Traverse.listTraverse
    val list = List("a", "b", "c")
    val expected = List(("a", 0), ("b", 1), ("c", 2))
    assert(T.zipWthIndex(list) == expected)
  }

  test("Option.toList") {
    val T = Traverse.optionTraverse
    assert(T.toList(Some(1)) == List(1))
  }

  test("Tree.toList") {
    val smallTree: Tree[Int] = Tree(1, List())
    val tallTree: Tree[Int] = Tree(1, List(Tree(2, List(Tree(3, List())))))
    val wideTree: Tree[Int] = Tree(1, List(Tree(2, List()), Tree(3, List())))
    val T = Traverse.treeTraverse
    assert(T.toList(smallTree) == List(1))
    assert(T.toList(tallTree) == List(1, 2, 3))
    assert(T.toList(wideTree) == List(1, 2, 3))
  }

  test("Option.reverse") {
    val T = Traverse.optionTraverse
    assert(T.reverse(Some(1)) == Some(1))
    assert(T.reverse(None) == None)
  }

  test("Tree.reverse") {
    val smallTree: Tree[Int] = Tree(1, List())
    val tallTree: Tree[Int] = Tree(1, List(Tree(2, List(Tree(3, List())))))
    val wideTree: Tree[Int] = Tree(1, List(Tree(2, List()), Tree(3, List())))
    val T = Traverse.treeTraverse
    assert(T.reverse(smallTree) == Tree(1, List()))
    assert(T.reverse(tallTree) == Tree(3, List(Tree(2, List(Tree(1, List()))))))
    assert(T.reverse(wideTree) == Tree(3, List(Tree(2, List()), Tree(1, List()))))
  }

  test("foldRight") {
    val T = Traverse.listTraverse
    val prop = Prop.forAll(Gen.listOf(Gen.int))(l => {
      T.foldLeft(l)(List[Int]())((b, a) => Cons(a, b)) == l.reverse
    })
    Prop.run(prop, testCases = 100)
  }

  test("fuse") {
    val T = Traverse.listTraverse
    val list = List(1, 2, 3, 4, 5, 6)
    val result = T.traverse(list)(a => Some(a): Option[Int])
    assert(result == Some(list))

    def lessThenFour(number: Int): Option[Int] = if (number < 4) Some(number) else None

    val result2 = T.fuse(list)(a => Some(a): Option[Int], a => lessThenFour(a))
    assert(result2 == (Some(list), None))

    val result3 = T.foldMap(list)(a => a)(Monoid.intAddition)
    assert(result3 == 21)

    val sumProd = Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication)
    val result4 = T.foldMap(list)(a => (a, a))(sumProd)
    assert(result4 == (21, 720))

    val sumLength = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
    val result5 = T.foldMap(list)(a => (a, 1))(sumLength)
    assert(result5 == (21, 6))

    val result6 = T.foldMap(List[Int]())(a => (a, 1))(sumLength)
    assert(result6 == (0, 0))
  }

  test("traverse compose") {
    val lT: Traverse[List] = Traverse.listTraverse
    val oT: Traverse[Option] = Traverse.optionTraverse
    val tmp = lT.compose(oT)
    val result = tmp.foldMap(List(Some(1), Some(2), Some(3)))(a => a)(Monoid.intAddition)
    assert(result == 6)
  }

  test("traverse compose 2") {
    val lT: Traverse[List] = Traverse.listTraverse
    val oT: Traverse[Option] = Traverse.optionTraverse
    val tT: Traverse[Tree] = Traverse.treeTraverse
    val tmp = lT.compose(oT).compose(tT)
    val structure = List(
      Some(
        Tree(1, List())
      ),
      Some(
        Tree(2, List(
          Tree(3, List())
        ))
      )
    )

    val result = tmp.foldMap(structure)(a => a)(Monoid.intAddition)
    assert(result == 6)

    val result2 = tmp.traverse(structure)(a => Some(a): Option[Int])
    assert(result2 == Some(structure))
  }

  object Role extends Enumeration {
    type Role = Value
    val Manager, Developer, ScrumMaster, ProductOwner = Value
  }

  import Role._

  case class Person(firstName: String, lastName: String, salery: Int, role: Role)

  test("traverse compose 3") {
    val structure = Map[String, List[Person]](
      "dep1" -> List(
        Person("Some", "Manager", 100000, Manager),
        Person("Some", "Developer", 20000, Developer),
        Person("Some", "Developer", 15000, Developer),
        Person("Some", "Developer", 25000, Developer),
        Person("Some", "Developer", 20000, Developer),
        Person("Some", "ScrumMaster", 70000, ScrumMaster),
        Person("Some", "ProductOwner", 80000, ProductOwner)
      ),
      "dep2" -> List(
        Person("Some", "Manager", 150000, Manager),
        Person("Some", "Developer", 20000, Developer),
        Person("Some", "Developer", 15000, Developer),
        Person("Some", "Developer", 25000, Developer),
        Person("Some", "Developer", 20000, Developer),
        Person("Some", "ScrumMaster", 100000, ScrumMaster),
        Person("Some", "ProductOwner", 110000, ProductOwner)
      ))

    val mT = Traverse.mapTraverse[String]
    val lT = Traverse.listTraverse
    val traverser = mT.compose(lT)

    val totalSalery = traverser.foldMap(structure)(a => a.salery)(Monoid.intAddition)
    assert(totalSalery == 770000)

    val maxSalery = traverser.foldMap(structure)(a => a.salery)(new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 max a2

      override def zero: Int = 0
    })
    assert(maxSalery == 150000)

    val saleryAndNumberOfPersons = traverser.foldMap(structure)(a => (a.salery, 1))(Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition))
    assert(saleryAndNumberOfPersons == (770000, 14))
  }
}
