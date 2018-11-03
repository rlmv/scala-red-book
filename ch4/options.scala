case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  def orElse[B >: A](obj: => Option[B]): Option[B] =
    this match {
      case None => obj
      case Some(a) => Some(a)
    }

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if (f(a)) Some(a) else None)

}



object Tests extends App {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some (xs.sum / xs.length)

  val someNone:Option[Int] = None
  val someOne:Option[Int] = Some(1)

  assert(someOne.map(_ + 1) == Some(2))
  assert(someNone.map(_ + 1) == None)

  assert(someOne.flatMap(x => Some(x + 4)) == Some(5))
  assert(someOne.flatMap(x => None:Option[Int]) == None)
  assert(someNone.flatMap(x => Some(x + 4)) == None)

  assert(someOne.getOrElse(2) == 1)
  assert(someNone.getOrElse(2) == 2)

  assert(someOne.orElse(Some(5)) == Some(1))
  assert(someNone.orElse(Some(5)) == Some(5))

  assert(someOne.filter(_ == 1) == Some(1))
  assert(someOne.filter(_ == 2) == None)
  assert(someNone.filter(_ == 1) == None)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).map(
      m => xs.map(x => math.pow(x - m, 2))
    ).flatMap(mean(_))


  assert(variance(List(1,2,3)) == Some(2.0/3.0))
  assert(variance(List()) == None)

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def insuranceRateQuote(age: Int, numTickets: Int): Double = 2.0

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a.flatMap(av => b.map(bv => f(av, bv)))
  }

  def parseInsuranceRateQuote(age: String, numTickets: String): Option[Double] = {
    val optAge = Try(age.toInt)
    val optNumTickets = Try(numTickets.toInt)
    map2(optAge, optNumTickets)(insuranceRateQuote)
  }

  assert(parseInsuranceRateQuote("34", "3") == Some(2.0))
  assert(parseInsuranceRateQuote("asf", "3") == None)


  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    val empty:Option[List[A]] = Some(List())
    l.foldRight(empty)((a,accum) => map2(a, accum)(_ :: _))
  }

  def parseInts(ss: List[String]): Option[List[Int]] =
    sequence(ss.map(x => Try(x.toInt)))

  assert(parseInts(List("1", "2")) == Some(List(1, 2)))
  assert(parseInts(List("1", "a")) == None)

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    val empty:Option[List[B]] = Some(Nil)
    as.foldRight(empty)((a, accum) => map2(f(a), accum)(_ :: _))
}

  def someIfOne(x: Int): Option[Int] =
    if (x == 1) Some(1)
    else None

  assert(traverse(List(1,1))(someIfOne) == Some(List(1,1)))
  assert(traverse(List(1,2))(someIfOne) == None)


}
