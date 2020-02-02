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
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      fa flatMap f

    override def unit[A](a: => A): Free[F, A] = Return(a)
  }

  val TailRec = freeMonad[Function0]

  type TailRecType[A] = Free[Function0, A]

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline {
        f(a)
      }
      case Suspend(r) => runTrampoline {
        f(r())
      }
      case FlatMap(y, g) => runTrampoline(y.flatMap(a => g(a).flatMap(f)))
    }
  }

  //@annotation.tailrec
  def step[F[_], A](free: Free[F, A])(implicit F: Monad2[F]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def run[F[_], A](free: Free[F, A])(implicit F: Monad2[F]): F[A] = step(free) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case _ => sys.error("Not possible since step has already matched")
  }


}
