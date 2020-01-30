package chapter12

import chapter3.{Cons, List}
import chapter4.{Either, Left, None, Option, Right, Some}
import chapter6.State

trait Monad2[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def unit[A](a: => A): F[A]

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad2 {
  def eitherMonad[E] = new Monad2[({type f[x] = Either[E, x]})#f] {
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case left@Left(l) => left
      case Right(a) => f(a)
    }

    override def unit[A](a: => A): Either[E, A] =
      Right(a)
  }

  def stateMonad[S] = new Monad2[({type lambda[x] = State[S, x]})#lambda] {
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa flatMap f

    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

  }
}