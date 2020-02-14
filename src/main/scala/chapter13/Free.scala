package chapter13

import java.util.concurrent.ExecutorService

import chapter12.Monad2
import chapter13.Free.Console.ConsoleIO
import chapter7.Par
import chapter7.Par.Par
import chapter3.{Cons, List, Nil}

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

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))

    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }

  object ConsoleReader {
    implicit val monad = new Monad2[ConsoleReader] {
      override def flatMap[A, B](fa: ConsoleReader[A])(f: A => ConsoleReader[B]): ConsoleReader[B] =
        fa flatMap f

      override def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_ => a)
    }
  }

  case class Buffers(in: List[String], out: List[String])

  case class ConsoleState[A](run: Buffers => (A, Buffers)) {
    def map[B](f: A => B): ConsoleState[B] =
      ConsoleState(b => {
        val (a, s) = run(b)
        (f(a), s)
      })

    def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] =
      ConsoleState(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  }

  object ConsoleState {
    implicit val monad: Monad2[ConsoleState] = new Monad2[ConsoleState] {
      override def flatMap[A, B](fa: ConsoleState[A])(f: A => ConsoleState[B]): ConsoleState[B] =
        fa flatMap f

      override def unit[A](a: => A): ConsoleState[A] = ConsoleState(b => {
        (a, b)
      })
    }
  }

  sealed trait Console[A] {
    def toPar: Par[A]

    def toThunk: () => A

    def toReader: ConsoleReader[A]

    def toState: ConsoleState[A]
  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)

    override def toReader: ConsoleReader[Option[String]] = {
      ConsoleReader(a=> Some(a))
    }

    override def toState: ConsoleState[Option[String]] = ConsoleState(b => {
      b.in match {
        case Nil => (None, Buffers(b.in.tail(), b.out))
        case (Cons(a, tail)) => (Some(a), Buffers(b.in.tail(), b.out))
      }
    })

    override def toThunk: () => Option[String] = () => run

    def run: Option[String] =
      try Some(readLine)
      catch {
        case e: Exception => None
      }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))

    override def toState: ConsoleState[Unit] = ConsoleState(b => {
      ((), Buffers(b.in, Cons(line, b.out)))
    })

    override def toThunk: () => Unit = () => println(line)

    override def toReader: ConsoleReader[Unit] = ConsoleReader(_ => ())
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

  val consoleToFunction0: Console ~> Function0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  val consoleToPar: Console ~> Par = new (Console ~> Par) {
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
    runFree[Console, Function0, A](a)(consoleToFunction0)(Monad2.functionMonad)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)(Monad2.parMonad)

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G, A] = Suspend {
        fg(a)
      }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline {
      translate(a)(new (Console ~> Function0) {
        def apply[A](c: Console[A]) = c.toThunk
      })
    }

  val consoleToReader: Console ~> ConsoleReader = new (Console ~> ConsoleReader) {
    override def apply[A](f: Console[A]): ConsoleReader[A] = f.toReader
  }

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console, ConsoleReader, A](io)(consoleToReader)(ConsoleReader.monad)

  val consoleToState = new (Console ~> ConsoleState) {
    override def apply[A](f: Console[A]): ConsoleState[A] = f.toState
  }

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] =
    runFree[Console, ConsoleState, A](io)(consoleToState)(ConsoleState.monad)

  type IO[A] = Free[Par, A]
  type IOConsole[A] = Free[Console, A]

  def unsafePerformIO[A](a: IO[A])(pool: ExecutorService): A =
    Par.run(pool)(run(a)(Monad2.parMonad))

  // Provides the `IO { ... }` syntax for synchronous IO blocks.
  def IO[A](a: => A): IO[A] = Suspend { Par.delay(a) }

  def Async[A](cb: (A => Unit) => Unit): IO[A] =
    Suspend(Par.async(cb))

}
