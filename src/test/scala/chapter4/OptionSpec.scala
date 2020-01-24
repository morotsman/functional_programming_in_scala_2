package chapter4

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {

  "Some(2).map(_+1)" should "result in Some(3)" in {
    assert(Some(2).map(_ + 1) == Some(3))
  }

  "None.map(_+1)" should "result in Some(3)" in {
    assert(None.map((v: Int) => v + 1) == None)
  }
  "Some(2).getOrElse(3)" should "result in Some(2)" in {
    assert(Some(2).getOrElse(3) == 2)
  }

  "None.getOrElse(3)" should "result in Some(3)" in {
    assert(None.getOrElse(3) == 3)
  }

  "Some(2).orElse(Some(3))" should "result in Some(2)" in {
    assert(Some(2).orElse(Some(3)) == Some(2))
  }

  "None.orElse(Some(3))" should "result in Some(3)" in {
    assert(None.orElse(Some(3)) == Some(3))
  }
  "Some(2).filter(_ == 2)" should "result in Some(2)" in {
    assert(Some(2).filter(_ == 2) == Some(2))
  }

  "Some(2).filter(_ != 2)" should "result in Some(2)" in {
    assert(Some(2).filter(_ != 2) == None)
  }

  "None.flatMap(v => Some(v))" should "result in None" in {
    assert(None.flatMap(v => Some(v)) == None)
  }

  "Some(2).flatMap(v => None)" should "result in None" in {
    assert(Some(2).flatMap(v => None) == None)
  }

  "Some(2).flatMap(v => Some(v))" should "result in Some(2)" in {
    assert(Some(2).flatMap(v => Some(v)) == Some(2))
  }


  def mean(xs: List[Double]): Option[Double] =
    if (xs.length == 0) None else Some(xs.sum / xs.length)

  def variance(xs: List[Double]): Option[Double] =
    for (
      m <- mean(xs);
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    ) yield v


  "map2(Some(2), Some(3))(_ + _)" should "result in Some(5)" in {
    assert(Option.map2(Some(2), Some(3))(_ + _) == Some(5))
  }

  "map2(Some(2), None)(_ + _)" should "result in None" in {
    assert(Option.map2(Some(2), None)(_ + _) == None)
  }

  "map2(None, Some(3))(_ + _)" should "result in None" in {
    assert(Option.map2(None: Option[Int], Some(3))(_ + _) == None)
  }
  "sequence(List())" should "result in Some(List()" in {
    assert(Option.sequence(List()) == Some(List()))
  }

  "sequence(List(Some(2), Some(3), Some(4)))" should "result in Some(List(2,3,4)" in {
    assert(Option.sequence(List(Some(2), Some(3), Some(4))) == Some(List(2, 3, 4)))
  }

  "sequence(List(None, Some(3), Some(4)))" should "result in None" in {
    assert(Option.sequence(List(None, Some(3), Some(4))) == None)
  }

  "sequence(List(Some(2), Some(3), None))" should "result in None" in {
    assert(Option.sequence(List(Some(2), Some(3), None)) == None)
  }
  "traverse(List(2, 3, 4))(a => if(a==5) None else Some(a) )" should "result in Some(List(2,3,4))" in {
    assert(Option.traverse(List(2, 3, 4))(a => if (a == 5) None else Some(a)) == Some(List(2, 3, 4)))
  }

  "traverse(List(2, 5, 4))(a => if(a==5) None else Some(a) )" should "result in None" in {
    assert(Option.traverse(List(2, 5, 4))(a => if (a == 5) None else Some(a)) == None)
  }
  "sequenceInTermsOfTraverse(List())" should "result in Some(List()" in {
    assert(Option.sequenceInTermsOfTraverse(List()) == Some(List()))
  }

  "sequenceInTermsOfTraverse(List(Some(2), Some(3), Some(4)))" should "result in Some(List(2,3,4)" in {
    assert(Option.sequenceInTermsOfTraverse(List(Some(2), Some(3), Some(4))) == Some(List(2, 3, 4)))
  }

  "sequenceInTermsOfTraverse(List(None, Some(3), Some(4)))" should "result in None" in {
    assert(Option.sequenceInTermsOfTraverse(List(None, Some(3), Some(4))) == None)
  }

  "sequenceInTermsOfTraverse(List(Some(2), Some(3), None))" should "result in None" in {
    assert(Option.sequenceInTermsOfTraverse(List(Some(2), Some(3), None)) == None)
    List(1, 2, 3).foldLeft(0)((a, b) => a + b)
  }


}
