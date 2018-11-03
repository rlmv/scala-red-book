>sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ns: List[Int]): Int = ns match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](al: List[A]): A = al match {
    case Nil => throw new Exception()
    case Cons(a, _) => a
  }

  def setHead[A](al: List[A], h: A): List[A] = al match {
    case Nil => throw new Exception()
    case Cons(a, as) => Cons(h, as)
  }

  def drop[A](al: List[A], n: Int): List[A] =
    if (n == 0) al
    else al match {
      case Nil => Nil
      case Cons(_, as) => drop(as, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) =>
      if (f(a)) dropWhile(as, f)
      else l
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def sum2(l: List[Int]) = foldRight(l, 0)((x, y) => x + y)

  def product2(l: List[Int]) = foldRight(l, 1)(_ * _)

  def length[A](l: List[A]) = foldRight(l, 0)((_, n) => n + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product3(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((accum, h) => Cons(h, accum))

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a, accum) => Cons(a, accum))

  def flatten[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil:List[A])(append2)

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((x, accum) => Cons(x+1, accum))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((x, bs) => Cons(f(x), bs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) Cons(h, filter(t)(f))
        else filter(t)(f)
    }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((a, bs) => List.append2(f(a), bs))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else List())

  def zipWith[A,B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] =  {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => throw new Exception("Lists are different lengths")
    }
  }

  def startsWith[A](l: List[A], subsequence: List[A]): Boolean =
    (l, subsequence) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(hl, tl), Cons(hs, ts)) => hl == hs && startsWith(tl, ts)
    }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

  def hasSubsequence[A](l: List[A], subsequence: List[A]): Boolean =
    if (l == Nil) false
    else startsWith(l, subsequence) || hasSubsequence(List.tail(l), subsequence)
}

object Tests extends App {
  val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
  assert(List.sum(l) == 6)
  assert(List.product(l) == 6)

  assert(List.product(List()) == 1)
  assert(List.sum(List(0,2,4,8)) == 14)

  assert(List.head(List(0,1,2)) == 0)

  assert(List.setHead(l, 5) == List(5, 2, 3))

  assert(List.drop(List(1,2,3,4,5,6,7), 4) == List(5,6,7))

  val lt2 = (x:Int) => x < 2
  assert(List.dropWhile(List(0,1,2), lt2) == List(2))
  assert(List.dropWhile(List(), lt2) == List())

  assert(List.append(List(1,2), List(3,4)) == List(1,2,3,4))

  assert(List.init(List(1)) == List())
  assert(List.init(List(1, 2)) == List(1))

  val l1 = List(1)
  val l2 = List(1,2,3,4)

  assert(List.sum(l2) == List.sum2(l2))
  assert(List.product(l2) == List.product2(l2))

  assert(List.foldRight(l2, Nil:List[Int])(Cons(_, _)) == l2)

  assert(List.length(l1) == 1)
  assert(List.length(l2) == 4)

  assert(List.sum3(l1) == 1)
  assert(List.sum3(l2) == 10)
  assert(List.product3(l2) == 24)

  assert(List.reverse(l2) == List(4,3,2,1))
  assert(List.reverse(l1) == l1)

  assert(List.append2(l2, l1) == List(1,2,3,4,1))

  assert(List.flatten(List(l1, l2, List(4, 4))) == List(1,1,2,3,4,4,4))

  assert(List.addOne(l2) == List(2,3,4,5))

  assert(List.map(l2)(_.toString) == List("1", "2", "3", "4"))

  assert(List.filter(l2)(x => x % 2 == 1) == List(1,3))
  assert(List.filter2(l2)(x => x % 2 == 1) == List(1,3))

  assert(List.flatMap(l2)(x => List(x,x)) == List(1,1,2,2,3,3,4,4))

  assert(List.zipWith(l2, l2)(_ + _) == List(2,4,6,8))
  assert(List.startsWith(l2, List(1,2,3)))
  assert(!List.startsWith(l2, List(3,4)))
  assert(List.tail(l2) == List(2,3,4))
  assert(List.hasSubsequence(l2, l1))
  assert(List.hasSubsequence(l2, List(2,3)))
  assert(!List.hasSubsequence(l2, List(3,2)))
}
