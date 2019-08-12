import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] { self =>

  // Implicit conversions to parser ops
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))
  implicit def parser[A](o: ParserOps[A]): Parser[A] = o.p

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  /**
    * Return the complete string matched by p
    */
  def slice[A](p: Parser[A]): Parser[String]

  /**
    * Recognizes zero or more instances of `p`
    * run(count(2, "a")("b123")) == Right(0)
    * run(count(2, "a")("a1a3")) == Right(2)
    */
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(List())

  /**
    * Recognizes one or more of `p`
    */
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
   map2(p, if (n > 0) listOfN(n - 1, p) else succeed(List()))(_ :: _)

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p1
      b <- p2
    } yield (a, b)

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2) map f.tupled

  def count[A](p: Parser[A]): Parser[Int] = p.many.map(_.length)

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Implicit class for creating infix operations
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): ParserOps[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): ParserOps[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice[A](p: Parser[A]): Parser[String] = self.slice(p)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }


}

object ListHelper {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): List[A] = {
    f(z) match {
      case Some((a, s)) => a :: unfold(s)(f)
      case _            => Nil
    }
  }

}

/**
  * Implementation
  */
case class ParseError(msg: String)

case class ParseState[+A](result: A, nextInput: String) {}

object Parser {

  type ParseResult[+A] = Either[ParseError, ParseState[A]]
  type Parser[+A] = String => ParseResult[A]

  def success[A](result: A, nextInput: String): ParseResult[A] =
    Right(ParseState(result, nextInput))

  def pure[A](result: A): Parser[A] = success(result, _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    input => p(input).map(state => state.copy(result = f(state.result)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    input => p(input).flatMap(state => f(state.result)(state.nextInput))

  // Combine a list of parsers, producing a new parser
  def fold[A, B](ps: List[Parser[A]])(base: B)(f: (B, A) => B): Parser[B] =
    input =>
      ps.foldLeft[ParseResult[B]](Right(ParseState(base, input)))((accum, p) =>
        accum.flatMap(oldState =>
          p(oldState.nextInput).map(newState =>
            newState.copy(result = f(oldState.result, newState.result)))))

  def takeWhile[A](p: Parser[A]): Parser[List[A]] = input => {
    val states = ListHelper.unfold(input)(input =>
      p(input) match {
        case Left(e)      => None
        case Right(state) => Some((state, state.nextInput))
    })

    states match {
      case head :: tail => success(states.map(_.result).reverse, head.nextInput)
      case _            => success(List.empty[A], input)
    }
  }
}

import Parser._

object ParsersImpl extends Parsers[ParseError, Parser] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(input).map(_.result)
  }

  implicit def string(s: String): Parser[String] =
    input =>
      input.startsWith(s) match {
        case true  => Right(ParseState(s, input.drop(s.length)))
        case false => Left(ParseError(s))
    }

//  def slice[A](p: Parser[A]): Parser[String] = ???

//  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = ???

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    input =>
      p1(input) match {
        case Left(e) => p2(input)
        case r       => r
    }
}

object Main extends App {

  import ParsersImpl._

  def assertI[A](s1: A, s2: A) =
    if (s1 != s2) {
      println(s"$s1 not equal to $s2")
      assert(s1 == s2)
    } else assert(s1 == s2)

  assertI(run(char('c'))("c"), Right('c'))
  assertI(run("ab12")("ab12"), Right("ab12"))
//  assertI(run(listOfN(3, "ab"))("ababab"), Right(List("ab", "ab", "ab")))
  // assertI(run("c" | "b")("cb"), Right("c"))
  // assertI(run("c" | "b")("bc"), Right("b"))
  //  assertI(run(count("c"))("ccccb"), Right("cccc"))

  val context = for {
    n <- regex("[0-9]+".r).map(_.toInt)
    _ <- listOfN(n, "a")
  } yield ()

}
