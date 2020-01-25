package chapter12

import chapter11.Functor
import chapter4.{Option, Some, None}
import chapter4.{Either, Left, Right}
import chapter8.{Gen, Prop}
import chapter3.{Cons, List, Nil}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  implicit val streamApplicative = new Applicative[Stream] {
    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa.zip(fb).map(a => f(a._1, a._2))

    override def unit[A](a: => A): Stream[A] =
      Stream.continually(a)
  }

  implicit val listApplicative = new Applicative[List] {
    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = (fa, fb) match {
      case (Cons(a, tla), Cons(b, tlb)) => Cons(f(a, b), map2(tla, tlb)(f))
      case _ => Nil
    }

    override def unit[A](a: => A): List[A] = List(a)
  }

  implicit def eitherApplicative[L] = new Applicative[({type f[r] = Either[L, r]})#f] {
    override def map2[A, B, C](fa: Either[L, A], fb: Either[L, B])(f: (A, B) => C): Either[L, C] = (fa, fb) match {
      case (Right(r1), Right(r2)) => Right(f(r1, r2))
      case (Left(l), _) => Left(l)
      case (_, Left(l)) => Left(l)
    }

    override def unit[A](a: => A): Either[L, A] = Right(a)
  }

  implicit val optionApplicative = new Applicative[Option] {
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

    override def unit[A](a: => A): Option[A] =
      Some(a)
  }

  implicit def validationApplicative[E]() = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Success(a), Failure(hd, tl)) => Failure(hd, tl)
      case (Failure(hd, tl), Success(a)) => Failure(hd, tl)
      case (Failure(hd1, tl1), Failure(hd2, tl2)) => Failure(hd1, tl1 ++ Vector(hd2) ++ tl2)
    }

    override def unit[A](a: => A): Validation[E, A] =
      Success(a)
  }

  val treeFuctor = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Tree(a, Nil) => Tree(f(a), Nil)
      case Tree(a, ts) => Tree(f(a), ts.map(t => map(t)(f)))
    }
  }

  def laws[A, T[_]](m: Applicative[T])(in: Gen[T[A]]): Prop = {
    val functor1 = Prop.forAll(in)(a => {
      m.map(a)(a => a) == a
    })

    val f: A => A = a => a
    val g: A => A = a => a

    val functor2 = Prop.forAll(in)(fa => {
      m.map(m.map(fa)(f))(g) == m.map(fa)(f compose g)
    })

    val leftIdentity = Prop.forAll(in)(fa => {
      m.map2(m.unit(()), fa)((_, a) => a) == fa
    })

    val rightIdentity = Prop.forAll(in)(fa => {
      m.map2(fa, m.unit(()))((a, _) => a) == fa
    })

    def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) = p match {
      case (a, (b, c)) => ((a, b), c)
    }

    val associativity = Prop.forAll(in)(fa => {
      m.product(m.product(fa, fa), fa) == m.map(m.product(fa, m.product(fa, fa)))(assoc)
    })

    def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
      (i, i2) => (f(i), g(i2))

    val naturality = Prop.forAll(in)(fa => {
      m.map2(fa, fa)(productF(f, g)) == m.product(m.map(fa)(f), m.map(fa)(g))
    })

    functor1 && functor2 && leftIdentity && rightIdentity && associativity && naturality
  }
}


trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(Cons(_, _)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((a, fbs) => map2(a, fbs)(Cons(_, _)))

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldRight(unit(Map[K, V]()))((kfv, fm) => {
      val fkv: F[(K, V)] = map(kfv._2)(v => (kfv._1, v))
      map2(fkv, fm)((a, b) => b + a)
    })
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        val fc: F[C] = self.map2(fa._1, fb._1)(f)
        val gc: G[C] = G.map2(fa._2, fb._2)(f)
        (fc, gc)
      }
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)((ga, gb) => G.map2(ga,gb)(f))

      override def unit[A](a: => A): F[G[A]] =
        self.unit(G.unit(a))
    }
  }

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def mapInTermsOfApply[A, B](fa: F[A])(ab: A => B): F[B] =
    apply(unit(ab))(fa)

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def curry2[A, B, C, D](f: (A, B, C) => D): A => B => C => D =
    a => b => c => f(a, b, c)

  def curry3[A, B, C, D, E](f: (A, B, C, D) => E): A => B => C => D => E =
    a => b => c => d => f(a, b, c, d)

  def map2InTermsOfApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fbc: F[B => C] = apply(unit(curry(f)))(fa)
    apply(fbc)(fb)
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fcd: F[C => D] = map2(fa, fb)((a, b) => curry2(f)(a)(b))
    apply(fcd)(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fde: F[D => E] = map3(fa, fb, fc)((a, b, c) => curry3(f)(a)(b)(c))
    apply(fde)(fd)
  }
}

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
}

trait Traverse[F[_]] {
  def traverse[M[_] : Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]]

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)
}

case class Tree[+A](head: A, tail: List[Tree[A]])


object Traverse {

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_], A, B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] = oa match {
      case Some(a) => M.map(f(a))(b => Some(b))
      case None => M.unit(None)
    }
  }

  val listTraverse = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, ml) => M.map2(f(a), ml)((b, l) => Cons(b, l)))

  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_], A, B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}
