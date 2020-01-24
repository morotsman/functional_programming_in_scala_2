package chapter2

import scala.annotation.tailrec

object chapter2 {

  def fib(n: Int): Int = {

    @tailrec def go(current: Int, prev1: Int, prev2: Int): Int = n match {
      case _ if current == n => prev2
      case _ => go(current + 1, prev1 + prev2, prev1)
    }

    go(1, 1, 0)
  }


  def isSorted[A](as: List[A], ordered: (A,A) => Boolean): Boolean = {
    def go(ls: List[A]): Boolean = ls match {
      case Nil => true
      case head :: Nil => true
      case (head :: first :: tail) if ordered(head, first) => go(ls.tail)
      case _ => false
    }

    go(as)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C =
    a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))



}


