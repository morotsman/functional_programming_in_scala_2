package chapter7

trait Par[+A] {}

object Par {
  def unit[A](a: A): Par[A] = ???
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
  def fork[A](a: => Par[A]): Par[A] = ???
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](pa: Par[A]): A = ???
}
