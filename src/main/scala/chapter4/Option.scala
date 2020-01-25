package chapter4


sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](b: B): B = this match {
    case None => b
    case Some(a) => a
  }

  def orElse[B >: A](o: Option[B]): Option[B] = this match {
    case None => o
    case Some(a) => this
  }

  def orElse2[B >: A](o: Option[B]): Option[B] =
    map(a => Some(a)).getOrElse(o)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if(f(v)) => this
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def isDefined() : Boolean = this match {
    case None => false
    case _ => true
  }
}

object Option {

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (Some(a), Some(b)) => Some(f(a,b))
    case _ => None
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(List()): Option[List[A]])((a, acc) => map2(a,acc)(_ :: _))

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List()): Option[List[B]])((a,acc) => map2(f(a), acc)(_ :: _))

  def sequenceInTermsOfTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => Some(a).getOrElse(None))

  def unit[A](a: A): Option[A] = Some(a)
}


case class Some[A](get: A) extends Option[A]

case object None extends Option[Nothing]



