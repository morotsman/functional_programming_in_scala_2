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

  def sequence_[A](fs: Stream[F[A]]): F[Unit] = foreachM(fs)(skip)

  def sequence_[A](fs: F[A]*): F[Unit] = sequence_(fs.toStream)

  def replicateMS[A](n: Int)(f: F[A]): F[List[A]] =
    Stream.fill(n)(f).foldRight(unit(List[A]()))(map2(_, _)(Cons(_, _)))

  def replicateM_[A](n: Int)(f: F[A]): F[Unit] =
    foreachM(Stream.fill(n)(f))(skip)

  def as[A, B](a: F[A])(b: B): F[B] = map(a)(_ => b)

  def skip[A](a: F[A]): F[Unit] = as(a)(())

  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
    if (b) as(fa)(true) else unit(false)

  def forever[A, B](a: F[A]): F[B] = {
    lazy val t: F[B] = a flatMap (_ => t)
    t
  }

  def while_(a: F[Boolean])(b: F[Unit]): F[Unit] = {
    lazy val t: F[Unit] = while_(a)(b)
    a flatMap (c => skip(when(c)(t)))
  }

  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[B] =
    l match {
      case h #:: t => f(z, h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] =
    skip {
      foldM(l)(z)(f)
    }

  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((u, a) => skip(f(a)))

  // syntax
  implicit def toMonadic[A](a: F[A]): Monadic[F, A] =
    new Monadic[F, A] {
      val F = Monad2.this;

      def get = a
    }
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

  def listMonad[A] = new Monad2[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    override def unit[A](a: => A): List[A] = List(a)
  }

  def optionMonad[A] = new Monad2[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  def stateMonad[S] = new Monad2[({type lambda[x] = State[S, x]})#lambda] {
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa flatMap f

    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
  }

  def composeM[G[_], H[_]](implicit G: Monad2[G], H: Monad2[H], T: Traverse[H]): Monad2[({type f[x] = G[H[x]]})#f] =
    new Monad2[({type f[x] = G[H[x]]})#f] {
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

      override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] = {
        G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
      }
    }
}

trait Monadic[F[_], A] {
  val F: Monad2[F]

  import F._

  def get: F[A]

  private val a = get

  def map[B](f: A => B): F[B] = F.map(a)(f)

  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)

  def **[B](b: F[B]) = F.map2(a, b)((_, _))

  def *>[B](b: F[B]) = F.map2(a, b)((_, b) => b)

  def map2[B, C](b: F[B])(f: (A, B) => C): F[C] = F.map2(a, b)(f)

  def as[B](b: B): F[B] = F.as(a)(b)

  def skip: F[Unit] = F.skip(a)

  def replicateM(n: Int) = F.replicateM(n, a)

  def replicateM_(n: Int) = F.replicateM_(n)(a)
}