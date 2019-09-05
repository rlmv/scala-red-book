package fpinscala.ch6.rng

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG extends App {

  val seeds = List(1, 100, 1000)
  val rngs = seeds.map(SimpleRNG(_))

  def results(f: RNG => Any) = {
    println(f)
    rngs.foreach(r => println(f(r)))
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    val r =
      if (n == Int.MinValue)
        0
      else if (n < 0)
        -n
      else
        n
    (r, nextRNG)
  }

  results(nonNegativeInt)

  results(double)

  def intDouble(rng1: RNG): ((Int, Double), RNG) = {
    val (x, rng2) = rng1.nextInt
    val (y, rng3) = double(rng2)
    ((x, y), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((x, y), rng2) = intDouble(rng)
    ((y, x), rng2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(), rng)
    else {
      val rngs =
        (0 until count).toList.scanLeft((0, rng))((r, _) => r._2.nextInt).tail
      (rngs.map(_._1), rngs(0)._2)
    }
  }

  println(ints(0)(rngs(0)))
  println(ints(3)(rngs(0)))

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, newRng) = s(rng)
      (f(a), newRng)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List[A]()))((r, accum) => map2(r, accum)(_ :: _))

  println(sequence(List.fill(3)(double))(SimpleRNG(1)))

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  println(ints(3)(rngs(0)))
  println(ints2(3)(rngs(0)))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  results(nonNegativeLessThan(100))

  def mapViaFlatMap[A, B](f: Rand[A])(g: A => B): Rand[B] =
    flatMap(f)(a => unit(g(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

}
