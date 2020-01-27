package chapter3

import chapter12.Applicative

import scala.annotation.tailrec

sealed trait List[+A] {


  def tail(): List[A] = this match {
    case Nil => Nil
    case Cons(a, as) => as
  }

  final def setHead[B >: A](na: B): List[B] = this match {
    case Nil => Nil
    case Cons(a, as) => Cons(na, as)
  }

  @tailrec
  final def drop(n: Int): List[A] = (this, n) match {
    case (_, c) if c == 0 => this
    case (Nil, _) => this
    case (Cons(a, as), _) => as.drop(n - 1)
  }

  def dropWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(a, as) if f(a) => as.dropWhile(f)
    case _ => this
  }

  def init(): List[A] = this match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, as) => Cons(a, as.init())
  }

  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case Cons(x, xs) => f(x, xs.foldRight(z)(f))
  }

  def append[B >: A](bs: List[B]): List[B] = (this, bs) match {
    case (Nil, xs) => xs
    case (as, Nil) => as
    case (Cons(a, as), Cons(x, xs)) => Cons(a, as.append(bs))
  }

  def appendInTermsOfFoldRight[B >: A](bs: List[B]): List[B] =
    foldRight(bs)((a, bs) => Cons(a, bs))

  def length(): Int =
    this.foldRight(0)((a, b) => b + 1)

  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case Nil => z
    case Cons(x, xs) => xs.foldLeft(f(z, x))(f)
  }

  def forEach(f: A => Unit): Unit = this match {
    case Nil =>
    case Cons(a, as) => f(a); as.forEach(f)
  }

  def reverse(): List[A] = {

    def go(acc: List[A], as: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(x, xs) => go(Cons(x, acc), xs)
    }

    go(Nil, this)
  }

  def foldLeftInTermOfFoldRight[B](z: B)(f: (A, B) => B): B =
    this.reverse().foldRight(z)((a, b) => f(a, b))

  final def foldRightInTermOfFoldLeft[B](z: B)(f: (A, B) => B): B =
    this.reverse().foldLeft(z)((a, b) => f(b, a))

  def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case Cons(a, as) => Cons(f(a), as.map(f))
  }

  def filter(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(a, as) if f(a) => Cons(a, as.filter(f))
    case Cons(_, as) => as.filter(f)
  }

  def flatMap[B](f: A => List[B]): List[B] = this match {
    case Nil => Nil
    case Cons(a, as) => f(a).append(as.flatMap(f))
  }

  def filterInTermsOfFlatMap(f: A => Boolean): List[A] =
    flatMap(a => if (f(a)) List(a) else Nil)

  def zipWith[B, C](list: List[B])(f: (A, B) => C): List[C] = (this, list) match {
    case (Nil, bs) => Nil
    case (as, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), as.zipWith(bs)(f))
  }

  def startsWith[B](bs: List[B]): Boolean = (this, bs) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(a, as), Cons(b, bs)) if (a == b) => as.startsWith(bs)
    case _ => false
  }

  def hasSubsequence[B](bs: List[B]): Boolean = (this, bs) match {
    case (Nil, Nil) => true
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(a, as), _) => if (this.startsWith(bs)) true else as.hasSubsequence(bs)
  }

  def mkString(separator: String): String =
    foldRight("")((a, b) => a.toString + separator + b)

  def toScalaList(): scala.List[A] = this match {
    case Nil => scala.List()
    case Cons(a, as) => a :: as.toScalaList()
  }

  def getHead(): A = this match {
    case Cons(a, as) => a
    case Nil => throw new RuntimeException("Head of empty list")
  }

  def forall(f: A => Boolean): Boolean = this match {
    case Cons(a, as) if f(a) => as.forall(f)
    case Cons(a, as) if !f(a) => false
    case _ => true
  }

  def exists(f: A => Boolean): Boolean = this match {
    case Nil => false
    case Cons(a, _) if f(a) => true
    case Cons(a, as) => as.exists(f)
  }

  def size(): Int = this match {
    case Nil => 0
    case Cons(a, as) => 1 + as.size()
  }

  def isEmpty(): Boolean = this match {
    case Nil => true
    case _ => false
  }

  def zip[B](lb: List[B]): List[(A, B)] = (this, lb) match {
    case (Cons(a, as), Cons(b, bs)) => Cons((a, b), as.zip(bs))
  }

  def contains[B >: A](aToFind: B): Boolean = this match {
    case Nil => false
    case Cons(a, as) if a == aToFind => true
    case Cons(a, as) => as.contains(aToFind)
  }

  def takeWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(a, _) if !f(a) => Nil
    case Cons(a, as) => Cons(a, as.takeWhile(f))
  }

}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, override val tail: List[A]) extends List[A]

object List {

  def fill[A](n: Int)(elem: => A): List[A] =
    if (n == 0) Nil else Cons(elem, fill(n - 1)(elem))


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def fromScalaList[A](as: scala.List[A]): List[A] = as match {
    case a :: as => Cons(a, fromScalaList(as))
    case _ => Nil
  }

  def concat[A](lists: List[List[A]]): List[A] =
    lists.foldLeft(Nil: List[A])((acc, b) => acc.append(b))

}
