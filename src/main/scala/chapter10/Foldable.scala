package chapter10

import chapter3.{Branch, Leaf, Tree}
import chapter4.{Option, None, Some}

import scala.annotation.tailrec

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f((a))))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())((a, b) => a :: b)
}

object FoldableList extends Foldable[List] {
  final override def foldRight[A, B](aas: List[A])(b: B)(f: (A, B) => B): B =
    foldLeft(aas.reverse)(b)((b, a) => f(a, b))

  @tailrec
  final override def foldLeft[A, B](aas: List[A])(b: B)(f: (B, A) => B): B = aas match {
    case Nil => b
    case a :: as => foldLeft(as)(f(b, a))(f)
  }
}

object FoldableIndexedSeq extends Foldable[IndexedSeq] {
  final override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as.reverse)(z)((b, a) => f(a, b))

  final override def foldLeft[A, B](aas: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    aas.foldLeft(z)(f)
}

object FoldableTree extends Foldable[Tree] {

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
}


object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }

}
