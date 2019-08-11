import scala.language.implicitConversions
import scala.language.higherKinds


trait Parsers[ParseError, Parser[+ _]] { self =>

  def char(c: Char): Parser[Char] // = string(c.toString).map(_.charAt(0))

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))
  implicit def parser[A](o: ParserOps[A]): Parser[A] = o.p

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  //  def and[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  /**
    * Recognizes zero or more instances of `p`
    * run(count(2, "a")("b123")) == Right(0)
    * run(count(2, "a")("a1a3")) == Right(2)
    */
  def many[A](p: Parser[A]): Parser[List[A]]

  def count[A](p: Parser[A]): Parser[Int] = p.many.map(_.length)

  // /**
  //   * Recognizes one or more of `p`
  //   */
  // def atLeastOneOf[A](p: Parser[A]): Parser[Int]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Implicit class for creating infix operations
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): ParserOps[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): ParserOps[B] = self.or(p, p2)

    // def &[B>:A](p2: Parser[B]): ParserOps[B] = self.and(p, p2)
    // def and[B>:A](p2: Parser[B]): ParserOps[B] = self.and(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)
  }
}

object ListHelper {

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): List[A] = {
    f(z) match {
      case Some((a, s)) => a :: unfold(s)(f)
      case _ => Nil
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
     val states = ListHelper.unfold(input)(input => p(input) match {
      case Left(e) => None
      case Right(state) => Some((state, state.nextInput))
     })

    states match {
      case head :: tail => success( states.map(_.result).reverse, head.nextInput)
      case _ => success(List.empty[A], input)
    }
  }
}

import Parser._

object ParsersImpl extends Parsers[ParseError, Parser] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(input).map(_.result)
  }

  def char(c: Char): Parser[Char] =
    input =>
      input.headOption match {
        case Some(h) if c == h => Right(ParseState(c, input.tail))
        case _                 => Left(ParseError(s"$c not found"))
    }

  implicit def string(s: String): Parser[String] =
    fold(s.toList.map(c => char(c)))("")(_ + _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = map(p)(f)

  def many[A](p: Parser[A]): Parser[List[A]] = ???

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
    input =>
      p1(input) match {
        case Left(e) => p2(input)
        case r       => r
    }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    map(fold(List.fill(n)(p))(List.empty[A])((a, b) => b :: a))(_.reverse)

//  def and[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???
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
  assertI(run("c" | "b")("cb"), Right("c"))
  assertI(run("c" | "b")("bc"), Right("b"))
  //  assertI(run(count("c"))("ccccb"), Right("cccc"))
}
