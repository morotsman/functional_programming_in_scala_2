package chapter4


trait Partial[+E, +A] {

  def map[B](f: A => B): Partial[E, B] = this match {
    case Errors(es) => Errors(es)
    case Success(a) => Success(f(a))
  }

  def flatMap[EE >: E, B](f: A => Partial[EE, B]): Partial[EE, B] = this match {
    case Success(a) => f(a)
    case Errors(e) => Errors(e)
  }

  def orElse[EE >: E, B >: A](b: => Partial[EE, B]): Partial[EE, B] = this match {
    case Errors(e) => b
    case Success(a) => Success(a)
  }

  def map2[EE >: E, B, C](b: Partial[EE, B])(f: (A, B) => C): Partial[EE, C] = (this, b) match {
    case (Success(a), Success(b)) => Success(f(a,b))
    case (Errors(e1), Errors(e2)) => Errors(e1 ++ e2)
    case (_, Errors(e2)) => Errors(e2)
    case (Errors(e1), _) => Errors(e1)
  }

  def forEach(f: A => Unit): Unit = this match {
    case Errors(e) =>
    case Success(a) => f(a)
  }
}

object Partial {

  def Test[A](a: => A): Partial[Exception, A] =
    try {
      Success(a)
    } catch {
      case e: Exception => Errors(List(e))
    }

  def sequence[E,A](es: List[Partial[E, A]]): Partial[E, List[A]] =
    es.foldRight(Success(List()): Partial[E,List[A]])((a,acc) => acc.map2(a)((c,d) => d :: c))

}

case class Errors[+E](get: List[E]) extends Partial[E, Nothing]

case class Success[+A](get: A) extends Partial[Nothing, A]

