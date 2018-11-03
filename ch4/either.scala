sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)


}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Tests extends App {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("list is empty")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  println(safeDiv(2, 4))
  println(safeDiv(4, 2))
  println(safeDiv(4, 0))

  val eExc: Either[Exception, Int] = Left(new Exception("failed"))
  val eOne: Either[Exception, Int] = Right(1)

  assert(eExc.map(_ + 1) == eExc)
  assert(eOne.map(_ + 1) == Right(2))

  assert(eExc.flatMap(x => Right(x + 1)) == eExc)
  assert(eOne.flatMap(x => Right(x + 1)) == Right(2))

  assert(eExc.orElse(Right(5)) == Right(5))
  assert(eOne.orElse(Right(5)) == eOne)

  assert(eOne.map2(eExc)(_ + _) == eExc)
  assert(eOne.map2(Right(2))(_ + _) == Right(3))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(List()))((a, accum) => for {
      bb <- accum
      b <- f(a)
    } yield b :: bb)

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(a => a)

}
