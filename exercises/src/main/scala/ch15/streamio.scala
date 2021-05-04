package fpinscala.ch15.streamio

import fpinscala.ch11.monad._

sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) =>
      s match {
        case h #:: t => recv(Some(h))(t)
        case xs      => recv(None)(xs)
      }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) =>
        Await {
          case None => recv(None)
          case i    => go(recv(i))
        }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  def |>[O2](p2: Process[O, O2]): Process[I, O2] =
    p2 match {
      case Halt() => Halt()
      case Emit(o2, t2) => Emit(o2, this |> t2)
      case Await(recv2) => this match {
        case Halt() => Halt() |> recv2(None)
        case Await(recv1) => Await((i: Option[I]) => recv1(i) |> p2)
        case Emit(o, t) => t |> recv2(Some(o))
      }
    }

  def map[O2](f : O => O2): Process[I, O2] = this |> Process.lift(f)

  def ++(p: => Process[I, O]): Process[I, O] =
    this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }

  def flatMap[O2](f : O => Process[I, O2]): Process[I, O2] =
    this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }

  // def zipWithIndex: Process[I, (O, Int)] =
  //   this

}

case class Emit[I, O](
    head: O,
    tail: Process[I, O] = Halt[I, O]()
) extends Process[I, O]

case class Await[I, O](
    recv: Option[I] => Process[I, O]
) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {

  def monad[I]: Monad[({ type f[x] = Process[I, x]})#f] =
    new Monad[({ type f[x] = Process[I, x]})#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)
      def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p.flatMap(f)
    }

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None    => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat

  def sum: Process[Double, Double] =
    loop(0.0)((i, s) => (i + s, i + s))

  def take[I](n: Int): Process[I, I]  = {
    def go(i: Int): Process[I, I] =
      Await {
        case Some(d) if n > i => Emit(d, go(i + 1))
        case _ => Halt()
      }
    go(0)
  }

  def drop[I](n: Int): Process[I, I]  =
    Await {
      case Some(d) if n > 0 => drop(n - 1)
      case Some(d) => Emit(d, drop(0))
      case _ => Halt()
    }

  def takeWhile[I](f: I => Boolean): Process[I, I] =
    Await {
      case Some(d) if f(d) => Emit(d, takeWhile(f))
      case _ => Halt()
    }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    def go(started: Boolean): Process[I, I] =
      Await {
        case Some(d) if started || !f(d) => Emit(d, go(true))
        case Some(d) => go(started)
        case _ => Halt()
      }
    go(false)
  }

  def count[I]: Process[I, Int] =
    loop(0)((_, s) => (s + 1, s + 1))

  def mean[I]: Process[Double, Double] =
    loop((0.0, 0)){
      case (i, (total, n)) => ((i + total) / (n + 1), (i + total, n + 1))
    }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    Await {
      case Some(i) => f(i, z) match {
        case (o, s2) => Emit(o, loop(s2)(f))
      }
      case _ => Halt()
    }

  //  def zip[I, O, O2](p1: Process[I, O], p2: Process[])


  // TODO: emit final "false"
  def exists[I](f: I=> Boolean): Process[I, Boolean] =
    lift(f) |> Process.dropWhile(!_) |> Process.take(1)

    // Await(_ match {
    //   case None => Halt()
    //   case Some(i) if f(i) => Emit(true)
    //   case _ => exists(f)
    // })
}

object Main extends App {

  def assertEq[A](s1: A, s2: A) =
    if (s1 != s2) {
      println(s"$s1 not equal to $s2")
      assert(s1 == s2)
    } else assert(s1 == s2)

  assertEq(Process.filter[Int](i => i < 2)(Stream(1, 2,3 )).toList, List(1))

  assertEq(Process.sum(Stream(1, 2, 3 )).toList, List(1, 3, 6))

  assertEq(Process.lift[Unit, Int](_  => 1)(Stream.continually(())).take(4).toList, List(1, 1, 1, 1))

  assertEq(Process.take(2)(Stream(1, 2, 3, 4, 5 )).toList, List(1, 2))
  assertEq(Process.drop(2)(Stream(1, 2, 3, 4, 5 )).toList, List(3, 4, 5))

  assertEq(Process.takeWhile[Int](_ % 2 == 1)(Stream(1, 2, 3, 4, 5 )).toList, List(1))
  assertEq(Process.dropWhile[Int](_ <= 2)(Stream(1, 2, 3, 4, 5 )).toList, List(3, 4,5))

  assertEq(Process.count(Stream(2, 2, 2, 2, 2 )).toList, List(1, 2, 3, 4, 5))
  assertEq(Process.mean(Stream(2, 2, 2, 2, 2 )).toList, List(2.0, 2.0, 2.0, 2.0, 2.0))
  assertEq(Process.mean(Stream(1, 2, 3, 4, 5 )).toList, List(1.0, 1.5, 2.0, 2.5, 3.0))

  assertEq((Process.drop(2) |> Process.mean) (Stream(1, 2, 3, 4, 5 )).toList, List(3, 3.5, 4))

  assertEq(Process.sum.map(_.toInt.toString)(Stream(1, 2, 3 )).toList, List("1", "3", "6"))

  //  println(Process.sum(Stream(1, 2, 3, 4, 5, 1 )).take(10).countEm.toList)

//  Process.zipWithIndex
  assertEq(Process.exists[Int](_ > 2)(Stream(1, 2, 3, 4, 5 )).toList, List(true))
}
