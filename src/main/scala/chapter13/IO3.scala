package chapter13


object IO3 {
  /*
   * As it turns out, there's nothing about this data type that is specific
   * to I/O, it's just a general purpose data type for optimizing tail calls.
   * Here it is, renamed to `TailRec`. This type is also sometimes called
   * `Trampoline`, because of the way interpreting it bounces back and forth
   * between the main `run` loop and the functions contained in the `TailRec`.
   */

  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad3[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)
    def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f
    def suspend[A](a: => TailRec[A]) =
      Suspend(() => ()).flatMap { _ => a }

  }

  def PrintLine(s: String): TailRec[Unit] =
    Suspend(() => {
      Return(println(s))
    })

  def ReadLine: TailRec[String] =
    Suspend(() => {
      readLine
    })



  @annotation.tailrec def run[A](t: TailRec[A]): A = t match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}
