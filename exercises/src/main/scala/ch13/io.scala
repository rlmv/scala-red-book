package fpinscala.ch13.io

import fpinscala.ch11.monad._
import fpinscala.ch7.par.Par

import scala.concurrent.duration.{TimeUnit}
import java.util.concurrent.{ExecutorService, Executors, Future, Callable}

object TailRecIO {

  sealed trait TailRec[A] {
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

    def flatMap[B](f: A => TailRec[B]) = FlatMap(this, f)

    def run: A = TailRec.run(this)
  }

  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], f: A => TailRec[B])
      extends TailRec[B]

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
}

object AsyncIO {
  sealed trait Async[A] {
    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))

    def flatMap[B](f: A => Async[B]) = FlatMap(this, f)

    def run: Par.Par[A] = Async.run(this)
  }

  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par.Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], f: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    def apply[A](a: => A): Async[A] = unit(a)

    def unit[A](a: => A): Async[A] = Suspend(Par.lazyUnit(a))

    def flatMap[A, B](io: Async[A])(f: A => Async[B]): Async[B] = io.flatMap(f)

    @annotation.tailrec
    def step[A](async: Async[A]): Async[A] = async match {
      case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f)     => step(f(x))
      case _                         => async

    }

    def run[A](async: Async[A]): Par.Par[A] = step(async) match {
      case Return(a)  => Par.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) =>
        x match {
          case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
          case _          => sys.error("Impossible; sys eliminates this case")
        }
    }

    def forever[A, B](a: Async[A]): Async[B] = {
      lazy val t: Async[B] = forever(a)
      a flatMap (_ => t)
    }

    def ReadLine: Async[String] = Async { readLine }
    def PrintLine(msg: String): Async[Unit] = Async { println(msg) }
  }
}

object Free {
  sealed trait Free[F[_], A] {
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
    def flatMap[B](f: A => Free[F, B]) = FlatMap(this, f)
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](sub: Free[F, A], f: A => Free[F, B])
      extends Free[F, B]

  def freeMonad[F[_]]: Monad[({ type f[A] = Free[F, A] })#f] =
    new Monad[({ type f[A] = Free[F, A] })#f] {

      def unit[A](a: => A): Free[F, A] = Return(a)

      def flatMap[A, B](free: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        free.flatMap(f)
    }

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par.Par, A]

  @annotation.tailrec
  def runTrampoline[A](t: TailRec[A]): A = t match {
    case Return(a)  => a
    case Suspend(a) => a()
    case FlatMap(x, f) =>
      x match {
        case Return(a)     => runTrampoline(f(a))
        case Suspend(a)    => runTrampoline(f(a()))
        case FlatMap(y, g) => runTrampoline(y flatMap (a => g(a) flatMap f))
      }
  }

  @annotation.tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => free

  }

  def run[F[_], A](free: Free[F, A])(implicit F: Monad[F]): F[A] =
    step(free) match {
      case Return(a)  => F.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) =>
        x match {
          case Suspend(r) => F.flatMap(r)(a => run(f(a)))
          case _          => sys.error("Impossible; sys eliminates this case")
        }
    }

  trait Translate[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }
  type ~>[F[_], G[_]] = Translate[F, G]

  def translate[F[_], G[_], A](free: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G, A] = Suspend { fg(a) }
    }
    runFree(free)(t)(freeMonad[G])
  }

  def runFree[F[_], G[_], A](
      free: Free[F, A]
  )(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _                      => sys.error("Impossilbe; step eliminates this case")
    }

}

object ConsoleIO {

  import Free._

  sealed trait Console[A] {
    def toPar: Par.Par[A]
    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run

    def run: Option[String] =
      try Some(readLine())
      catch { case e: Exception => None }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  val consoleToFunction0 =
    new (Console ~> Function0) {
      def apply[A](a: Console[A]): Function0[A] = a.toThunk
    }

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A): Function0[A] = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]): Function0[B] =
      f(a())
  }

  val consoleToPar =
    new (Console ~> Par.Par) {
      def apply[A](a: Console[A]): Par.Par[A] = a.toPar
    }

  implicit val parMonad = new Monad[Par.Par] {
    def unit[A](a: => A): Par.Par[A] = Par.lazyUnit(a)
    def flatMap[A, B](a: Par.Par[A])(f: A => Par.Par[B]): Par.Par[B] =
      Par.flatMap(a)(f)
  }

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline(translate(a)(new (Console ~> Function0) {
      def apply[A](a: Console[A]) = a.toThunk
    }))


  val program = for {
    _ <- Console.printLn("Type something, please...")
    ln <- Console.readLn
    _ <- Console.printLn(s"Got $ln")
  } yield ()

  def runExample = runFree(program)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par.Par[A] =
    runFree(a)(consoleToPar)


  trait Source{
    def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit
  }

}

object Main extends App {

  val executor: ExecutorService = Executors.newFixedThreadPool(10)

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  import AsyncIO._

  def converter: Async[Unit] =
    for {
      _ <- Async.PrintLine("Enter a temp in Fahrenheit")
      d <- Async.ReadLine.map(_.toDouble)
      _ <- Async.PrintLine(fahrenheitToCelsius(d).toString)
    } yield ()

  Async.forever(Async.PrintLine("hi"))

  val p = (for {
    _ <- Async.PrintLine("Hi")
    _ <- Async.PrintLine("again")
  } yield ()).run

  Par.run(executor)(p).get()

  executor.shutdown()

  ConsoleIO.runExample

}
