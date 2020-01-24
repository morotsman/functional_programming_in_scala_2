package chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a,b))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e)}

  def sequence[E,A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(List()): Either[E,List[A]])((a,acc) => acc.map2(a)((c,d) => d :: c))


  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List()): Either[E,List[B]])((a,acc) => acc.map2(f(a))((c,d) => d :: c))

}
