package chapter13

import chapter12.Monad2

object IO1 {
  sealed trait IO[A] {
    self =>

    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] {
        def run = {
          f(self.run)
        }
      }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] {
        override def run: B = {
          f(self.run).run
        }
      }

  }

  object IO extends Monad2[IO] {
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    override def unit[A](a: => A): IO[A] = new IO[A] {
      def run = a
    }

    def apply[A](a: => A): IO[A] = unit(a)

    def ReadLine: IO[String] = IO {
      readLine
    }

    def PrintLine(message: String): IO[Unit] = IO {
      println(message)
    }

    def ref[A](a: A): IO[IORef[A]] = IO {
      new IORef(a)
    }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO {
        value = a; a
      }

      def get: IO[A] = IO {
        value
      }

      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }

    val echo = ReadLine.flatMap(PrintLine)

    val readInt = ReadLine.map(_.toInt)

    val readInts = map2(readInt, readInt)((_, _))

    val read10Lines = replicateM(10, ReadLine)
  }
}

