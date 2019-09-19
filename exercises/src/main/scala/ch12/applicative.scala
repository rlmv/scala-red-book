package fpinscala.ch12.applicative

import scala.language.higherKinds
import fpinscala.ch6.state._
import fpinscala.ch11.monad._
import fpinscala.ch10.monoid._
import java.util.Date

trait Applicative[F[_]] extends Functor[F] { self =>

  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as match {
      case h :: t =>
        map2(f(h), filterM(t)(f))(
          (check, filtered) => if (check) h :: filtered else filtered
        )
      case Nil => unit(Nil)
    }

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // Alternate primitive for Applicative
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  // Defined in terms of apply
  def mapV[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // Defined in terms of apply
  def map2V[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f curried))(fa))(fb))(fc)

  def product[G[_]](G: Applicative[G]) =
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      def map2[A, B, C](a: (F[A], G[A]), b: (F[B], G[B]))(
          f: (A, B) => C
      ): (F[C], G[C]) =
        (self.map2(a._1, b._1)(f), G.map2(a._2, b._2)(f))
    }

  def compose[G[_]](G: Applicative[G]) =
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(
          f: (A, B) => C
      ): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }

}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {

  def applicativeValidation[E] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)
      def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C
      ): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(ha, ta), Failure(hb, tb)) =>
            Failure(ha, ta ++ Vector(hb) ++ tb)
          case (_, e @ Failure(_, _)) => e
          case (e @ Failure(_, _), _) => e
        }
    }

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] = {
    //  try {
    import java.text._
    Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    //  } catch {
    //    Failure("birthdate must be in the form yyyy-MM-dd")

    // }

  }
  def validPhone(phone: String): Validation[String, String] =
    if (phone.matches("[0-9]{10}")) Success(phone)
    else Failure("phone number must be 10 digits")

  case class WebForm(name: String, birthdate: Date, phone: String)

  def validWebForm(
      name: String,
      birthdate: String,
      phone: String
  ): Validation[String, WebForm] =
    applicativeValidation[String].map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone)
    )(WebForm)
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
      implicit G: Applicative[G]
  ): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_], A, B](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Id(f(a)))(IdApplicative).value

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    }

  override def foldMap[A, M](as: F[A])(f: A => M)(m: Monoid[M]): M =
    traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(
      monoidApplicative(m)
    )

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (S, A) => (S, B)): (F[B], S) =
    traverseS(fa)(
      (a: A) =>
        for {
          s1 <- State.get[S]
          (s2, b) = f(s1, a)
          _ <- State.set(s2)
        } yield b
    ).run(s)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((s, a) => (s + 1, (a, s)))._1
  // traverseS(fa)(a => for {
  //   i <- State.get[Int]
  //   _ <- State.set(i + 1)
  // } yield (a, i)).run(0)._1

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A])((s, a) => ((a :: s), ()))._2.reverse
  // traverseS(fa)(a => for {
  //   l <- State.get[List[A]]
  //   _ <- State.set(a :: l)
  // } yield ()).run(Nil)._2.reverse

  // TODO: 12.16
  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((s, a) => (f(s, a), ()))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(
      g: A => G[B],
      h: A => H[B]
  )(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (g(a), h(a)))(
      G.product(H)
    )

  def compose[G[_]](
      implicit G: Traverse[G]
  ): Traverse[({ type f[x] = F[G[x]] })#f] =
    new Traverse[({ type f[x] = F[G[x]] })#f] {
      override def traverse[H[_], A, B](fa: F[G[A]])(f: A => H[B])(
          implicit H: Applicative[H]
      ): H[F[G[B]]] = self.traverse(fa)(G.traverse(_)(f))
    }
}

object IdApplicative extends Applicative[Id] {
  def unit[A](a: => A): Id[A] = Id(a)
  def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
    Id(f(fa.value, fb.value))
}

object ListTraverse extends Traverse[List] {

  override def traverse[G[_], A, B](
      fa: List[A]
  )(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
    fa.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))

}

case class Tree[+A](head: A, tail: List[Tree[A]])

object TreeTraverse extends Traverse[Tree] {

  override def traverse[G[_], A, B](
      ta: Tree[A]
  )(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
    G.map2(
      f(ta.head),
      ListTraverse.traverse(ta.tail)(taInner => traverse(taInner)(f))
    )((h, t) => Tree(h, t))
}

object OptionTraverse extends Traverse[Option] {

  override def traverse[G[_], A, B](
      fa: Option[A]
  )(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
    fa match {
      case Some(a) => G.map(f(a))(Some(_))
      case None    => G.unit(None)
    }

}

object Main extends App {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa zip fb map (f tupled)
  }

}
