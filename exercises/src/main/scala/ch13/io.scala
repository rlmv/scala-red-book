package fpinscala.ch13.io

import fpinscala.ch11.monad._

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => IO[B]) = FlatMap(this, f)

  def run: A = IO.run(this)

}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], f : A => IO[B]) extends IO[B]

object IO extends Monad[IO] {
  def apply[A](a: => A): IO[A] = unit(a)

  def unit[A](a: => A): IO[A] = Suspend(() => a)

  def flatMap[A, B](io: IO[A])(f: A => IO[B]): IO[B] = io.flatMap(f)

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(a) => a()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(a) => run(f(a()))
      case FlatMap(y, g) => run (y flatMap (a => g(a) flatMap f))
    }
  }

  def forever[A, B](a: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(a)
    a flatMap (_ => t)
  }

  def ReadLine : IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
}

object Main extends App {

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- IO.PrintLine("Enter a temp in Fahrenheit")
    d <- IO.ReadLine.map(_.toDouble)
    _ <- IO.PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  IO.forever(IO.PrintLine("hi"))

  (for {
    _ <- IO.PrintLine("Hi")
    _ <- IO.PrintLine("again")
  } yield ()).run
}
