import scala.language.implicitConversions
import scala.language.higherKinds

trait Parsers[ParseError, Parser[+_]] { self =>

  def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  // def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  //  def and[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  // def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // /**
  //   * Recognizes zero or more instances of `p`
  //   * run(count(2, "a")("b123")) == Right(0)
  //   * run(count(2, "a")("a1a3")) == Right(2)
  //   */
  // def manyOf[A](p: Parser[A]): Parser[Int]

  // /**
  //   * Recognizes one or more of `p`
  //   */
  // def atLeastOneOf[A](p: Parser[A]): Parser[Int]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Implicit class for creating infix operations
  case class ParserOps[A](p: Parser[A]) {
    // def |[B>:A](p2: Parser[B]): ParserOps[B] = self.or(p, p2)
    // def or[B>:A](p2: Parser[B]): ParserOps[B] = self.or(p, p2)

    // def &[B>:A](p2: Parser[B]): ParserOps[B] = self.and(p, p2)
    // def and[B>:A](p2: Parser[B]): ParserOps[B] = self.and(p, p2)

  }
}

/**
  * Implementation
  */
case class ParseError(msg: String)

case class ParseState[+A](result: A, nextInput: String) {

}

object Parser {
  type Parser[+A] = String => Either[ParseError, ParseState[A]]

  // Combine a list of parsers, producing a new parser
  def fold[A, B](ps: List[Parser[A]])(base: B)(f: (B, A) => B): Parser[B] = ???

}
import Parser._

object ParsersImpl extends Parsers[ParseError, Parser]{

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(input).map(_.result)
  }

  def char(c: Char): Parser[Char] = input => {
    val i = input.indexWhere(_ == c)
    if (i < 0) Left(ParseError(s"$c not found"))
    else Right(ParseState(c, input.splitAt(i + 1)._2))
  }

  implicit def string(s: String): Parser[String] =
    fold(s.toList.map(c => char(c)))("")(_ + _)

//  def and[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???
}

object Main extends App {

  import ParsersImpl._

  def assertI[A](s1: A, s2: A) =
    if (s1 !=s2) {
      println(s"$s1 not equal to $s2")
      assert(s1 == s2)
    }
    else assert(s1 == s2)

  assertI(run(char('c'))("c"), Right('c'))
//  assertI(run(char('c') & char('b'))("cb"), Right('c'))
  assertI(run("12")("ab12"), Right("12"))
}
