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
    val r = if (n == Int.MinValue)
      0
    else if (n < 0)
      -n
    else
      n
    (r, nextRNG)
  }

  results(nonNegativeInt)

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, nextRNG)
  }

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
      val rngs = (0 until count).toList.scanLeft((0, rng))((r, _) => r._2.nextInt).tail
      (rngs.map(_._1), rngs(0)._2)
    }
  }

  println(ints(0)(rngs(0)))
  println(ints(3)(rngs(0)))

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, newRng) = s(rng)
      (f(a), newRng)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)


}
