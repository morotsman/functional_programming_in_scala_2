package chapter10

import chapter3.{Branch, Leaf, Tree}
import chapter3.{List, Cons, Nil}
import chapter4.{None, Option, Some}

trait Foldable[F[_]] {
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  private def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as)(curry(f))(new Monoid[B => B] {
      override def op(a1: B => B, a2: B => B): B => B =
        a1 compose a2

      override def zero: B => B = b => b
    })(z)
  }

  private def wrap[A, B, C](f: B => A => B): A => B => B =
    a => b => f(b)(a)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as)(wrap(curry(f)))(new Monoid[B => B] {
      override def op(a1: B => B, a2: B => B): B => B =
        a1 andThen a2

      override def zero: B => B = b => b
    })(z)
  }

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())((a, b) => Cons(a, b))
}

object FoldableList extends Foldable[List] {
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Nil => mb.zero
    case Cons(a, as) => mb.op(f(a), foldMap(as)(f)(mb))
  }
}

object FoldableIndexedSeq extends Foldable[IndexedSeq] {
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case a +: as => mb.op(f(a), foldMap(as)(f)(mb))
    case _ => mb.zero
  }
}

object FoldableTree extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
}


object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }
}
