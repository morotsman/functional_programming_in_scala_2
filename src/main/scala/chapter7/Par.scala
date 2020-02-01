package chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import chapter3.{List, Cons}


object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def unit[A](a: A): Par[A] =
    es => UnitFuture(a)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get(), bf.get()))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(() => a(es).get())

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
    val plla: Par[List[List[A]]]= sequence(lpla)
    map(plla)(lla => lla.flatMap(la => la))
  }

  def run[A](s: ExecutorService)(pa: Par[A]): Future[A] = pa(s)
}
