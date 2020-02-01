package chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Future, TimeUnit}

import chapter3.{Cons, List}
import chapter4.{Either, Left, Right}


object Par {

  sealed trait Future[A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      override def apply(k: A => Unit): Unit = k(a)
    }

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      override private[chapter7] def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => new Future[B] {
      override private[chapter7] def apply(cb: B => Unit): Unit =
        pa(es)(a => choices(a)(es)(b => cb(b)))

    }

  def flatMap[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    chooser(pa)(f)

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      override private[chapter7] def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    val fcurry = (a: A, b: B) => (c: C) => f(a, b, c)
    val pcTod = map2(pa, pb)(fcurry)
    map2(pcTod, pc)((cTod, c) => cTod(c))
  }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val fcurry = (a: A, b: B, c: C) => (d: D) => f(a, b, c, d)
    val pdToe = map3(pa, pb, pc)(fcurry)
    map2(pdToe, pd)((pdToe, d) => pdToe(d))
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((pa, pla) => map2(pa, pla)((a, la) => Cons(a, la)))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val lpb: List[Par[B]] = ps.map(asyncF(f))
    sequence(lpb)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val ff: A => List[A] = (a: A) => if (f(a)) List(a) else List()
    val lpla: List[Par[List[A]]] = as.map(asyncF(ff))
    val plla: Par[List[List[A]]] = sequence(lpla)
    map(plla)(lla => lla.flatMap(la => la))
  }

  def parFold[A, B](as: IndexedSeq[A])(b: B)(f: A => Par[B])(m: (B, B) => B): Par[B] = {
    if (as.isEmpty) {
      unit(b)
    } else if (as.size == 1) {
      f(as.head)
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(fork(parFold(l)(b)(f)(m)), fork(parFold(r)(b)(f)(m)))(((a, b) => m(a, b)))
    }
  }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def run[A](es: ExecutorService)(pa: Par[A]): A = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)

    pa(es) { a =>
      ref.set(a);
      latch.countDown()
    }

    latch.await()
    ref.get()
  }
}
