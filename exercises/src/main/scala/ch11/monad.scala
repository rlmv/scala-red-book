package fpinscala.ch11.monad

import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A, B](a: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f : (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}


object OptionMonad extends Monad[Option] {

  def unit[A](a: A) = Some(a)

  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
    case Some(a) => f(a)
    case None => None
  }
}

object ListMonad extends Monad[List] {

  def unit[A](a: A): List[A] = List(a)
  def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

}

object Main extends App {
  println("hi")
}
