package chapter11

import chapter8.{Gen, Prop, SGen}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(ab => ab._1), map(fab)(ab => ab._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }


}

object Functor {
  object Laws {
    def preserveStructure[A, T[_]](m: Functor[T])(in: Gen[T[A]]): Prop =
      Prop.forAll(in)(a => {
        m.map(a)(a => a) == a
      })

  }
}

object ListFunctor extends Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B): List[B] =
    fa.map(f)

  object Laws {
    def preserveStructure[A](m: Functor[List])(in: SGen[List[A]]): Prop =
      Prop.forAll(in)(a => {
        m.map(a)(a => a) == a
      })

  }
}

object OptionFunctor extends Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
    fa.map(f)
}
