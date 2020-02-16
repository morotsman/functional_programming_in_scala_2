package chapter15

sealed trait Process[I, O] {

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }
}

object Process {

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case _ => Halt()
      }

    go(0.0)
  }

  def take[I](n: Int): Process[I, I] = {
    def go(index: Int): Process[I, I] =
      Await {
        case Some(i) if index < n => Emit(i, go(index + 1))
        case _ => Halt()
      }

    go(0)
  }

  def drop[I](n: Int): Process[I, I] = {
    def go(index: Int): Process[I, I] =
      Await {
        case Some(i) if index < n => go(index + 1)
        case Some(i) => Emit(i, go(index))
        case None => Halt()
      }

    go(0)
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = Await {
    case Some(i) if f(i) => Emit(i, takeWhile(f))
    case _ => Halt()
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = Await {
    case Some(i) if f(i) => dropWhile(f)
    case Some(i) => Emit(i, dropWhile(f))
    case None => Halt()
  }

  def count[I]: Process[I, Int] = {
    def go(index: Int): Process[I, Int] = Await {
      case None => Halt()
      case Some(i) => Emit(index, go(index + 1))
    }

    go(1)
  }

  def mean: Process[Double, Double] = {
    def go(sum: Double, count: Double): Process[Double, Double] = Await {
      case None => Halt()
      case Some(d) => Emit((sum + d) / (count + 1.0), go(sum + d, count + 1.0))
    }

    go(0.0, 0.0)
  }

  def emit[I,O](head: O,
                tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Emit(head, tail)

  def await[I,O](f: I => Process[I,O], fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Await[I,O] {
      case Some(i) => f(i)
      case None => fallback
    }

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
    await((i: I) => f(i,z) match {
      case (o,s2) => emit(o, loop(s2)(f))
    })

  
}


case class Emit[I, O](
                       head: O,
                       tail: Process[I, O] = Halt[I, O]()
                     ) extends Process[I, O]

case class Await[I, O](
                        recv: Option[I] => Process[I, O]
                      ) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]
