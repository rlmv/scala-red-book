package fpinscala.ch11.monad

import scala.language.higherKinds
import fpinscala.ch9.parse._
import fpinscala.ch6.state._
import fpinscala.ch12.applicative._

trait Functor[F[_]] {

  def map[A, B](a: F[A])(f: A => B): F[B]

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

trait Monad[F[_]] extends Functor[F] with Applicative[F] { self =>

  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  override def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as match {
      case h :: t =>
        map2(f(h), filterM(t)(f))(
          (check, filtered) => if (check) h :: filtered else filtered
        )
      case Nil => unit(Nil)
    }

  override def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  // Kleisli arrows
  // Using compose as an associative operation, and unit, Monads have a Monoid
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // flatMap can be implemented in terms of compose
  def flatMapC[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  //  compose(f, compose(g, h))  == a => f(a).flatMap(g).flatMap(h)
  //  a => f(a).flatMap(g(_).flatMap(h) a => f(a).flatMap(g).flatMap(h)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(fa => fa)

  def flatMapJ[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  // Not possible (from Applicative chapter)
  //
  // def compose[G[_]](G: Monad[G]) =
  //   new Monad[({ type f[x] = F[G[x]] })#f] {
  //     def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
  //     def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
  //       self.flatMap(fa)(G.flatMap(_)
  //   }

  // But can be done if G is Traversable
  def compose[G[_]](G: Monad[G], T: Traverse[G]) =
    new Monad[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
        val F = self
        F.flatMap(fa)(
          (ga: G[A]) => F.map(T.traverse(ga)(f)(F))(G.join)
        )
      }
    }
}

object Monad {
  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State.unit(a)
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st.flatMap(f)
  }
}

object OptionMonad extends Monad[Option] {

  def unit[A](a: => A) = Some(a)
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
    case Some(a) => f(a)
    case None    => None
  }
}

object ListMonad extends Monad[List] {
  def unit[A](a: => A): List[A] = List(a)
  def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object IdMonad extends Monad[Id] {
  def unit[A](a: => A) = Id(a)
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
}

case class Reader[R, A](run: R => A) {

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
    Reader(r => f(run(r)).run(r))

  def map[B](f: A => B): Reader[R, B] =
    Reader(r => f(run(r)))
}

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      st.flatMap(f)
  }
}

object Main extends App {
  for {
    a <- Id("Hello")
    b <- Id("World")
  } yield a + b

  val intReader = Reader.readerMonad[Int]

  val program = for {
    r <- Reader[Int, String](i => i.toString)
  } yield (r, s"first: $r")

  println(program.run(5))
}
