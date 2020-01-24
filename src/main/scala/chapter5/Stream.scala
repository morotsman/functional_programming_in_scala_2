package chapter5

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    def go(acc: List[A], as: Stream[A]): List[A] = as match {
      case Empty => acc
      case Cons(hd, tl) => go(hd() :: acc, tl())
    }

    go(Nil, this).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case _ if n == 0 => Empty
    case Empty => Empty
    case Cons(hd, tl) => Stream.cons(hd(), tl().take(n - 1))
  }

  def takeInTermsOfUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (_, a) if a == 0 => None
      case (Empty, _) => None
      case (Cons(hd, tl), _) => Some((hd(), (tl(), n - 1)))
    }

  def drop(n: Int): Stream[A] = this match {
    case _ if n == 0 => this
    case Empty => Empty
    case Cons(hd, tl) => tl().drop(n - 1)
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(hd, tl) if f(hd()) => Stream.cons(hd(), tl().takeWhile(f))
    case _ => Empty
  }

  def takeWhileInTermsOfUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(hd, tl) if f(hd()) => Some(hd(), tl())
      case _ => None
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(hd, tl) => f(hd(), tl().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(hd, tl) if p(hd()) => tl().forAll(p)
    case Empty => true
    case _ => false
  }

  def takeWhileInTermsOfFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, acc) => if (f(a)) Stream.cons(a, acc) else acc)

  def headOptionInTermsOfFoldRight: Option[A] =
    foldRight(None: Option[A])((a, acc) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, acc) => Stream.cons(f(a), acc))

  def mapInTermsOfUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(hd, tl) => Some((f(hd()), tl()))
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, acc) => if (f(a)) Stream.cons(a, acc) else acc)

  def append[B >: A](sb: => Stream[B]): Stream[B] =
    foldRight(sb)((a, acc) => Stream.cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, acc) => f(a).append(acc))

  def peek(f: A => Unit): Stream[A] = this match {
    case Empty => Empty
    case Cons(hd, tl) => f(hd()); Stream.cons(hd(), tl().peek(f))
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, bs)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, bs)) {
      case (Empty, Cons(hd, tl)) => Some(((None, Some(hd())), (Empty, tl())))
      case (Cons(hd, tl), Empty) => Some(((Some(hd()), None), (tl(), Empty)))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case _ => None

    }

  def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(hd, tl) if f(hd()) => Some(hd())
    case Cons(hd, tl) => tl().find(f)
  }

  def zip[B](bs: Stream[B]): Stream[(A,B)] =
    zipWith(bs)((_,_))

  def startsWith[B >: A](bs: Stream[B]): Boolean =
    !zipAll(bs).takeWhile(_._2 != None).exists(v => v._1 != v._2)

  def tails(): Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(hd, tl) => Some((Stream.cons(hd(), tl()), tl()))
    }.append(Stream(Stream()))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Empty => Stream[B](z)
    case Cons(h,t) => {
      val right@Cons(newZ,_) = t().scanRight(z)(f)
      val b = f(h(),newZ())
      Stream.cons(b, right)
    }
  }

  def hasSubsqequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}


case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def unit[A](a: A): Stream[A] =
    cons(a, Empty)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = Stream.cons(1, ones)

  def continually[A](a: => A): Stream[A] = Stream.cons(a, continually(a))

  def onesInTermsOfUnfold: Stream[Int] =
    unfold(1)(s => Some(s, s))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def constantInTermsOfUnfold[A](a: A): Stream[A] =
    unfold(a)(s => Some(s, s))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fromInTermsOfUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def fibs(): Stream[Int] = {
    def go(prev1: Int, prev2: Int): Stream[Int] = Stream.cons(prev2, go(prev1 + prev2, prev1))

    go(1, 0)
  }

  def fibsInTermsOfUnfold(): Stream[Int] =
    unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val next = f(z)
    f(z) match {
      case None => Empty: Stream[A]
      case Some((as, ss)) => Stream.cons(next.get._1, unfold(next.get._2)(f))
    }
  }
}
