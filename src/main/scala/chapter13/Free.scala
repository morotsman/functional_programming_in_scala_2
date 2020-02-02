package chapter13

import chapter12.Monad2
import language.higherKinds
import language.postfixOps

object Free {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)

    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]]: Monad2[({type f[a] = Free[F, a]})#f] = new Monad2[({type f[a] = Free[F, a]})#f] {
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ???

    override def unit[A](a: => A): Free[F, A] = ???
  }

}
