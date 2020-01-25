package chapter10

import chapter4.{Option, None, Some}
import chapter8.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object Monoid {

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) =
      (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

    override def zero: (A, B) = (a.zero, b.zero)
  }

  def wordCound(string: String): Int = {
    foldMapV((string.trim()).toIndexedSeq, wcMonoid)(a => if (a.isWhitespace) Part("", 0, "") else Stub(a + "")) match {
      case Part(l, words, r) =>
        (if (l.length > 0) 1 else 0) + words + (if (r.length > 0) 1 else 0)
      case Stub(char) =>
        if (char.length > 0) 1 else 0
    }
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(chars), Part(lStub, words, rStub)) =>
        Part(chars + lStub, words, rStub)
      case (Part(lStub, words, rStub), Stub(chars)) =>
        Part(lStub, words, rStub + chars)
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
        if (rStub1.length > 0 || lStub2.length > 0) {
          Part(lStub1, words1 + words2 + 1, rStub2)
        } else {
          Part(lStub1, words1 + words2, rStub2)
        }
      case (Stub(char1), Stub(char2)) =>
        val result = Stub(char1 + char2)
        result
    }

    override def zero: WC = Stub("")
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

    def go(as: IndexedSeq[A]): B =
      if (as.isEmpty) m.zero
      else if (as.length == 1) f(as(0))
      else {
        val (a1, a2) = as.splitAt(as.size / 2)
        m.op(go(a1), go(a2))
      }

    go(v)
  }

  val orderedMonoid: Monoid[Option[Int]] = new Monoid[Option[Int]] {
    override def op(a1: Option[Int], a2: Option[Int]): Option[Int] = (a1, a2) match {
      case (Some(i1), Some(i2)) if i1 <= i2 => Some(i1.max(i2))
      case _ => None
    }

    override def zero: Option[Int] = Some(Int.MinValue)
  }

  def ordered(v: List[Int]): Boolean = {
    def f: Int => Option[Int] = i => Some(i)

    foldMap(v, orderedMonoid)(f).isDefined
  }

  def inside(number: Int, low: Int, high: Int): Boolean =
    (number >= low) && (number <= high)

  val orderedMonoidV: Monoid[Option[(Int, Int)]] = new Monoid[Option[(Int, Int)]] {
    override def op(a1: Option[(Int, Int)], a2: Option[(Int, Int)]): Option[(Int, Int)] = {
      (a1, a2) match {
        case (Some((min1, max1)), Some((min2, max2))) if (max1 <= min2) => {
          Some(min1.min(min2), max1.max(max2))
        }
        case _ => {
          None
        }
      }
    }

    override def zero: Option[(Int, Int)] = Some(Int.MaxValue, Int.MinValue)
  }

  def orderedV(v: IndexedSeq[Int]): Boolean = {
    def f: Int => Option[(Int, Int)] = i => Some((i, i))

    foldMapV(v, orderedMonoidV)(f).isDefined
  }

  def foldRightInTermsOfFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val aToB: A => B => B = a => b => f(a, b)

    foldMap(as, endoMonoid[B])(aToB)(z)
  }


  def foldRightInTermsOfFoldMap2[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val monoid: Monoid[B => B] = new Monoid[B => B] {
      override def op(a1: B => B, a2: B => B): B => B =
        a1 compose a2

      override def zero: B => B = b => b
    }

    val aToB: A => B => B = a => b => f(a, b)

    foldMap(as, monoid)(aToB)(z)
  }


  def foldLeftInTermsOfFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val monoid: Monoid[B => B] = new Monoid[B => B] {
      override def op(a1: B => B, a2: B => B): B => B =
        a1 andThen a2

      override def zero: B => B = b => b
    }

    val aToB: A => B => B = a => b => f(a, b)

    foldMap(as, monoid)(aToB)(z)
  }


  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = List()
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A =
      a => a1(a2(a))

    override def zero: A => A =
      a => a
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B =
      a => b.op(a1(a), a2(a))

    override def zero: A => B =
      a => b.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val f: A => Map[A, Int] = a => Map(a -> 1)
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(f)
  }

  object Laws {
    def associativeLaw[A](m: Monoid[A])(in: Gen[A]): Prop =
      Prop.forAll(in)(a => {
        m.op(m.op(a, a), a) == m.op(a, m.op(a, a))
      })

    def identityLaw[A](m: Monoid[A])(in: Gen[A]): Prop =
      Prop.forAll(in)(a => {
        println(a)
        m.op(m.zero, a) == a
      })
  }

}

