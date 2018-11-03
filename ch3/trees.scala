sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def max(t: Tree[Int]): Int =
    t match {
      case Leaf(v) => v
      case Branch(l, r) => max(l) max max(r)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B =
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def max2(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ max _)

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf[B](f(v)))((l, r) => Branch[B](l,r))

}


object Tests extends App {
  val t:Tree[Int] = Branch(
    Branch(Leaf(1), Leaf(2)), Leaf(3))

  assert(Tree.size(t) == 5)
  assert(Tree.max(t) == 3)
  assert(Tree.depth(t) == 3)
  assert(Tree.map(t)(_.toString) == Branch(
    Branch(Leaf("1"), Leaf("2")), Leaf("3")))

  assert(Tree.size2(t) == 5)
  assert(Tree.max2(t) == 3)
  assert(Tree.depth2(t) == 3)
  assert(Tree.map2(t)(_.toString) == Branch(
    Branch(Leaf("1"), Leaf("2")), Leaf("3")))


}
