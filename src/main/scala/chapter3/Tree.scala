package chapter3

sealed trait Tree[+A] {

  def size(): Int = this match {
    case Leaf(_) => 1
    case Branch(l , r) => 1 + l.size() + r.size()
  }

  def maximum[B >: A](max: (B,B) => B): B = this match {
    case Leaf(v) => v
    case Branch(l, r) => max(l.maximum(max), r.maximum(max))
  }

  def depth(): Int = this match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + l.depth().max(r.depth())
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def fold[B](vf: A => B)(bf: (B, B) => B): B = this match {
    case Leaf(v) => vf(v)
    case Branch(l, r) => bf(l.fold(vf)(bf),r.fold(vf)(bf))
  }

  def sizeInTermsOfFold(): Int =
    fold(a => 1)((l,r) => 1 + r + l)

  def depthInTermsOfFold(): Int =
    fold(a => 1)((l,r) => 1 + l.max(r))

  def maximumInTermsOfFold[B >: A](max: (B,B) => B): B =
    fold((a: B) => a)((l,r) => max(l,r))

  def mapInTermsOfFold[B](f: A => B): Tree[B] =
    fold(v => Leaf(f(v)): Tree[B])((l,r) => Branch(l,r))

}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
