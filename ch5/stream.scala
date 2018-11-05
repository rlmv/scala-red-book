import Stream._
sealed trait Stream[+A] {
  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  def headOption: Option[A] =
    this match {
      case Cons(h, t) => Some(h())
      case _ => None
    }

  def drop(n: Int): Stream[A] = {
    if (n == 0) this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n - 1)
    }
  }

  def take(n: Int): Stream[A] = {
    if (n == 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Empty => false
      case Cons(h, t) => p(h()) || t().exists(p)
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) Stream.cons(a, b)
      else empty
    )

  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, accum) => cons(f(h), accum))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, accum) => if (p(h)) cons(h, accum) else accum)

  def append[B>:A](b: => B): Stream[B] =
    foldRight(Stream(b))((h, t) => cons(h, t))

  def ++[B>:A](other: Stream[B]): Stream[B] =
    foldRight(other)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]) =
    foldRight(empty[B])((a, bs) => f(a) ++ bs)

  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this)(_ match {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    })

  def take2(n: Int): Stream[A] =
    Stream.unfold((this, n))(_ match {
      case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m - 1)))
      case _ => None
    })

  def takeWhile3(f: A => Boolean): Stream[A] =
    Stream.unfold(this)(_ match {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2))({
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), Empty)))
      case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case _ => None
    })

  def hasSubsequence[A](s: Stream[A]): Boolean =
    this.zipAll(s)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(m: Int, n: Int): Stream[Int] = cons(m, go(n, m + n))
    go(0, 1)
  }

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => Empty
    }
  }

  def fibs2: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def constant2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s)))

}
object Tests extends App {
  val e = Stream()
  val s = Stream(1,2,3,2,1)
  val t = Stream(4,5,6,7,8)

  assert(s.drop(2).toList == List(3,2,1))
  assert(s.take(2).toList == List(1,2))

  assert(s.exists(_ == 2))
  assert(!s.exists(_ == 4))

  assert(s.exists2(_ == 2))
  assert(!s.exists2(_ == 4))

  assert(s.forAll(_ < 4))
  assert(!s.forAll(_ > 4))

  assert(s.takeWhile(_ < 2).toList == List(1))
  assert(s.takeWhile2(_ < 2).toList == List(1))
  assert(s.takeWhile3(_ < 2).toList == List(1))

  assert(s.headOption2 == Some(1))
  assert(e.headOption2 == None)

  assert(s.map(_.toString).toList == List("1", "2", "3", "2", "1"))
  assert(s.map2(_.toString).toList == List("1", "2", "3", "2", "1"))

  assert(s.filter(_ > 2).toList == List(3))

  assert(s.append(6).toList == s.toList :+ 6)

  assert(s.take(2).flatMap(i => Stream(i, i)).toList == List(1, 1, 2, 2))

  assert(ones.take(5).toList == List(1,1,1,1,1))

  ones.takeWhile(_ == 1)

  assert(constant(2).take(3).toList == List(2,2,2))
  assert(constant2(2).take(3).toList == List(2,2,2))

  assert(from(2).take(3).toList == List(2,3,4))
  assert(from2(2).take(3).toList == List(2,3,4))

  assert(Stream.fibs.take(6).toList == List(0,1,1,2,3,5))
  assert(Stream.fibs2.take(6).toList == List(0,1,1,2,3,5))

  println(s.take2(2).toList)
  assert(s.take2(2).toList == List(1, 2))




}
