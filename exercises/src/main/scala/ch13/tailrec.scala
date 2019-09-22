package fpinscala.ch13.tailrec

import fpinscala.ch11.monad._

sealed trait TailRec[A] {
  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => TailRec[B]) = FlatMap(this, f)

  def run: A = TailRec.run(this)
}

case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A]
case class FlatMap[A, B](sub: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {
  def apply[A](a: => A): TailRec[A] = unit(a)

  def unit[A](a: => A): TailRec[A] = Suspend(() => a)

  def flatMap[A, B](io: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
    io.flatMap(f)

  @annotation.tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a)  => a
    case Suspend(a) => a()
    case FlatMap(x, f) =>
      x match {
        case Return(a)     => run(f(a))
        case Suspend(a)    => run(f(a()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }
}
