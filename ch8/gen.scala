// trait Gen[A] {

// }

case class Gen[A](sample: State[RNG, A])

object Gen {

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(x: Int, y: Int): Gen[Int] = ???

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {

  import Prop._

  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  def &&(p: Prop): Prop = ???
}

object Main extends App {

  val intList = Gen.listOf(Gen.choose(0, 100))

  val prop =
    Gen.forAll(intList)(ns => ns.reverse.reverse == ns) &&
      Gen.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

}

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(si => {
      val (a, s) = this.run(si)
      g(a).run(s)
    })

  def map[B](g: A => B): State[S, B] =
    this.flatMap(a => State.unit(g(a)))

  def map[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))

}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

}

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
