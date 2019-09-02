import scala.math
import scala.math.max
import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  val zero: A
}

object Instances {

  val stringConcatenation: Monoid[String] = new Monoid[String] {
    def op(a: String, b: String): String = a + b
    val zero: String = ""
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a + b
    val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a * b
    val zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a | b
    val zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a & b
    val zero: Boolean = true
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a: A => A, b: A => A): A => A = a andThen b
    val zero: A => A = a => a
  }

  def concatenate[A](l: List[A], m: Monoid[A]): A =
    l.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldMapV[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 0) m.zero
    else if (as.length == 1) m.op(f(as.head), m.zero)
    else {
      val (b, c) = as.splitAt(math.floor(as.length).toInt)
      m.op(foldMap(b, m)(f), foldMap(c, m)(f))
    }

  sealed trait OrderState
  case object Empty extends OrderState
  case class Concrete(max: Int, isOrdered: Boolean = true) extends OrderState

  def isOrdered(ns: List[Int]): Boolean = {
    val orderMonoid: Monoid[OrderState] = new Monoid[OrderState] {
      val zero = Empty
      def op(a: OrderState, b: OrderState): OrderState = (a, b) match {
        case (Empty, Empty) => Empty
        case (a, Empty)     => a
        case (Empty, b)     => b
        case (a @ Concrete(_, _), b @ Concrete(_, _)) =>
          Concrete(max(a.max, b.max),
                   a.isOrdered && b.isOrdered && a.max <= b.max)
      }
    }
    foldMapV(ns, orderMonoid)(Concrete(_)) match {
      case Concrete(_, isOrdered) => isOrdered
      case Empty                  => true
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC): WC =
      (a, b) match {
        case (Part(als, ac, ars), Part(bls, bc, brs)) =>
          Part(als, ac + bc + wordCount(ars + bls), brs)

        case (Part(als, ac, ars), Stub(brs)) =>
          Part(als, ac + wordCount(ars + brs), "")

        case (Stub(als), Part(bls, bc, brs)) =>
          Part("", bc + wordCount(als + bls), brs)

        case (Stub(als), Stub(brs)) =>
          Stub(als + brs)
      }
    val zero: WC = Stub("")
  }

  def wordCount(c: String): Int = if (c.trim == "") 0 else 1

  def countWords(chars: String): Int =
    foldMapV(chars.to[List], wcMonoid)(c => Stub(c.toString)) match {
      case Stub(chars)      => wordCount(chars)
      case Part(ls, wc, rs) => wordCount(ls) + wc + wordCount(rs)
    }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(_ :: _)
  }

  object FoldableList extends Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldLeft(as.reverse)(z)((a, b) => f(b, a))

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as match {
        case head :: tail => foldLeft(tail)(f(z, head))(f)
        case Nil          => z
      }
  }

  object FoldableStream extends Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object FoldableTree extends Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(value)  => f(value, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(value)  => f(z, value)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(v)      => f(v)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }
  }

  object FoldableOption extends Foldable[Option] {
    override def foldMap[A, B](o: Option[A])(f: A => B)(mb: Monoid[B]): B =
      o match {
        case Some(a) => f(a)
        case None    => mb.zero
      }

    def foldLeft[A, B](o: Option[A])(z: B)(f: (B, A) => B): B =
      o match {
        case Some(a) => f(z, a)
        case None    => z
      }

    def foldRight[A, B](o: Option[A])(z: B)(f: (A, B) => B): B =
      o match {
        case Some(a) => f(a, z)
        case None    => z
      }
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      val zero: (A, B) = (ma.zero, mb.zero)
      def op(x: (A, B), y: (A, B)): (A, B) =
        (ma.op(x._1, y._1), mb.op(x._2, y._2))
    }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      val zero: A => B = _ => mb.zero
      def op(f: A => B, g: A => B): A => B =
        a => mb.op(f(a), f(a))
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      val zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero)((acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero))))
    }

  def bag[A](as: List[A]): Map[A, Int] =
    foldMap(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

}

object Main extends App {

  def assertEq[A](s1: A, s2: A) =
    if (s1 != s2) {
      println(s"$s1 not equal to $s2")
      assert(s1 == s2)
    } else assert(s1 == s2)

  import Instances._

  assert(concatenate[String](List("1", "b"), stringConcatenation) == "1b")
  assert(
    foldMapV(List(1, 2, 3, 4, 5), stringConcatenation)(_.toString) == "12345")

  //  assertEq(countWords("test two"), 2)
  assert(isOrdered(List()))
  assert(isOrdered(List(8)))
  assert(isOrdered(List(1, 2, 3)))
  assert(!isOrdered(List(1, 5, 3)))

  assertEq(FoldableOption.toList(Some(1)), List(1))

  assertEq(FoldableOption.toList(None), Nil)

  assertEq(bag(List(1, 2, 3, 4, 3, 4)), Map(1 -> 1, 2 -> 1, 3 -> 2, 4 -> 2))
}
