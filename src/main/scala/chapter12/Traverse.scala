package chapter12

import chapter10.{Foldable, Monoid}
import chapter11.Functor
import chapter3.{Cons, List, Nil}
import chapter4.{None, Option, Some}
import chapter6.State
import chapter6.State.{get, set}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  type Const[M, B] = M

  def traverse[M[_] : Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]]

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Applicative.idApplicative.unit(f(a))).run()

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(Applicative.monoidApplicative(mb))


  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad2.stateMonad)

  def zipWthIndex2[A](ta: F[A]): F[(A, Int)] = {
    val result: State[Int, F[(A, Int)]] = traverseS(ta)(a => State({ (i: Int) => ((a, i), i + 1) }))
    result.run(0)._1
  }

  def zipWthIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => (for (
      i <- get[Int];
      _ <- set(1 + i)
    ) yield (a, i))).run(0)._1

  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- get[List[A]]
      _ <- set(Cons(a, as): List[A])
    } yield ())).run(List[A]())._2.reverse()

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b)).run(s)

  def toListInTermsOfMapAccum[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), Cons(a, s)))._2.reverse()

  def zipWithIndexInTermsOfMapAccum[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.getHead(), as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => {
      val b = f(s, a)
      (b, b)
    })._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes")
      case (a, Cons(b, bs)) => ((a, b), bs)
    })._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None: Option[B]), Nil)
      case (a, Cons(b, bs)) => ((a, Some(b)), bs)
    })._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    (mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None: Option[A], b), Nil)
      case (b, Cons(a, as)) => ((Some(a), b), as)
    })._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G.product(H))

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_], A, B](fga: F[G[A]])(f: A => M[B])(implicit M: Applicative[M]): M[F[G[B]]] =
        self.traverse(fga)(ga => G.traverse(ga)(a => f(a)))
    }
  }
}

object Traverse {

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_], A, B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] = oa match {
      case Some(a) => M.map(f(a))(b => Some(b))
      case None => M.unit(None)
    }
  }

  val listTraverse = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, ml) => M.map2(f(a), ml)((b, l) => Cons(b, l)))

  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_], A, B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

  def mapTraverse[K] = new Traverse[({type f[v] = Map[K,v]})#f] {
    override def traverse[M[_], A, B](fa: Map[K, A])(f: A => M[B])(implicit M: Applicative[M]): M[Map[K, B]] =
      fa.foldRight(M.unit(Map[K, B]()))((ka, ml) => M.map2(f(ka._2), ml)((b, m) => m + (ka._1 -> b)))
  }
}
