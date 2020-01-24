package chapter11

import chapter4.Option
import chapter5.Stream
import chapter6.State
import chapter8.{Gen, Prop}
import chapter9.{ParserError, Parsers}


trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((fa, fla) => flatMap(fa)(a => map(fla)(la => a :: la)))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    val lfa: List[F[A]] = List.fill(n)(ma)
    sequence(lfa)
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))

  def flatMapInTermsOfCompose[A, B](fa: F[A])(afb: A => F[B]): F[B] = {
    val ufa: Unit => F[A] = _ => fa
    val ufb: Unit => F[B] = compose(ufa, afb)

    ufb(())
  }

  /*
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((x,y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x),y)(_ :: _) else y)(x))
  */

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val flab: F[List[(A, Boolean)]] = sequence(ms.map(a => map(f(a))(b => (a, b))))
    map(flab)(lab => lab.filter(ab => ab._2).map(ab => ab._1))
  }

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def flatMapInTermsOfJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(a => f(a)))

}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
  }

  def parserMonad[P[+ _]](p: Parsers[ParserError, P]): Monad[P] = new Monad[P] {
    override def unit[A](a: A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: A): Option[A] =
      Option.unit(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: A): Stream[A] =
      Stream.unit(a)

    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
      fa.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: A): List[A] =
      List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    override def unit[A](a: A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa flatMap f
  }

  object Laws {
    def leftIdentity[A, F[_]](m: Monad[F])(in: Gen[F[A]]): Prop =
      Prop.forAll(in)(fa => {
        m.flatMap(fa)(m.unit) == fa
      })

    def rightIdentity[A, F[_]](m: Monad[F])(in: Gen[A]): Prop =
      Prop.forAll(in)(a => {
        val f: A => F[A] = fa => m.unit(a)
        m.flatMap(m.unit(a))(f) == f(a)
      })

    def associative[A, F[_]](m: Monad[F])(in: Gen[F[A]]): Prop =
      Prop.forAll(in)(fa => {
        val f: A => F[A] = a => m.unit(a)
        val g: A => F[A] = a => m.unit(a)

        m.flatMap(m.flatMap(fa)(f))(g) == m.flatMap(fa)(a => m.flatMap(f(a))(g))
      })

    def leftIdentityCompose[A, F[_]](m: Monad[F])(in: Gen[A]): Prop =
      Prop.forAll(in)(a => {
        val f: A => F[A] = a => m.unit(a)

        m.compose(f, m.unit[A])(a) == f(a)
      })

    def rightIdentityCompose[A, F[_]](m: Monad[F])(in: Gen[A]): Prop =
      Prop.forAll(in)(a => {
        val f: A => F[A] = fa => m.unit(a)
        m.compose(m.unit[A], f)(a) == f(a)
      })

    def associativeCompose[A, F[_]](m: Monad[F])(in: Gen[A]): Prop =
      Prop.forAll(in)(a => {
        val f: A => F[A] = a => m.unit(a)
        val g: A => F[A] = a => m.unit(a)
        val h: A => F[A] = a => m.unit(a)

        m.compose(m.compose(f, g), h)(a) == m.compose(f, m.compose(g, h))(a)
      })

  }

}

case class Id[A](value: A) {
  val monad = Monad.idMonad

  def map[B](f: A => B): Id[B] = monad.map(this)(f)

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] =
    Reader.ReaderMonad.map(this)(f)

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
    Reader(r => f(this.run(r)).run(r))

}

object Reader {
  def ReaderMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: A): Reader[R, A] =
      Reader(_ => a)

    override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      fa.flatMap(f)

  }

  def ask[R]: Reader[R, R] = Reader(r => r)
}


