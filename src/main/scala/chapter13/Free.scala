package chapter13

import chapter12.Monad2
import chapter7.Par
import chapter7.Par.Par

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

  val TailRec: Monad2[({type f[a] = Free[Function0, a]})#f] =
    freeMonad[Function0]

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

  @annotation.tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def run[F[_], A](free: Free[F, A])(implicit F: Monad2[F]): F[A] = step(free) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case _ => sys.error("Not possible since step has already matched")
  }

  sealed trait Console[A] {
    def toPar: Par[A]

    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)

    override def toThunk: () => Option[String] = () => run

    def run: Option[String] =
      try Some(readLine)
      catch {
        case e: Exception => None
      }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))

    override def toThunk: () => Unit = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0: Console ~> Function0 = new (Console ~> Function0){
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  val consoleToPar: Console ~> Par = new (Console ~> Par){
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad2[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible, step eliminates these cases")
    }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)
}
