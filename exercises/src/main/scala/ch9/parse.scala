import scala.language.implicitConversions
import scala.language.higherKinds

import scala.math.pow
import scala.util.matching.Regex

import ParserTypes._

object ParserTypes {

  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _             => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _                => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(get, i) => Success(get, i + n)
      case _               => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean = false)
      extends Result[Nothing]

}

object Parser extends Parsers[Parser] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(get, _) => Right(get)
      case Failure(get, _) => Left(get)
    }

  def succeed[A](a: A): Parser[A] = (l: Location) => Success(a, 0)

  implicit def string(s: String): Parser[String] =
    (l: Location) =>
      if (l.remainder.startsWith(s)) Success(s, s.length)
      else Failure(l.toError(s"Expected: $s"))

  implicit def regex(r: Regex): Parser[String] =
    (l: Location) =>
      r.findFirstIn(l.remainder) match {
        case Some(s) => Success(s, s.length)
        case None    => Failure(l.toError(s"No match for: $r"))
    }

  def slice[A](p: Parser[A]): Parser[String] =
    (l: Location) => {
println(l)
      p(l) match {
        case Success(_, i)     => Success(l.input.slice(l.offset, l.offset + i), i)
        case e @ Failure(_, _) => e


      }
    }

  def attempt[A](p: Parser[A]): Parser[A] = (l: Location) => p(l).uncommit

  def errorMessage(e: ParseError): String = e.stack.head._2
  def errrorLocation(e: ParseError): Location = e.stack.head._1

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    (l: Location) => {
      p(l) match {
        case Success(get, i) =>
          f(get)(l.advanceBy(i))
            .addCommit(i != 0)
            .advanceSuccess(i)
        case e @ Failure(_, _) => e
      }
    }

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    (l: Location) => p(l).mapError(_.label(l, msg))

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    (l: Location) => p(l).mapError(_.push(l, msg))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    (l: Location) =>
      p1(l) match {
        case Failure(_, false) => p2(l)
        case r                 => r
    }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(
    List((this, msg))
  )

  def advanceBy(n: Int): Location =
    copy(offset = offset + n)

  def remainder: String = input.slice(offset, input.length)
}

case class ParseError(stack: List[(Location, String)]) {

  def push(l: Location, msg: String): ParseError =
    copy(stack = (l, msg) :: stack)

  def label(l: Location, msg: String): ParseError =
    ParseError(latestLoc.map((_, msg)).toList)

  def latestLoc: Option[Location] =
    latest.map(_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}

trait Parsers[Parser[+ _]] { self =>

  def errrorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  // Implicit conversions to parser ops
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))
  implicit def parser[A](o: ParserOps[A]): Parser[A] = o.p

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  /**
    * Return the complete string matched by p
    */
  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // Add a string to a parser
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // Nest multiple labels
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def succeed[A](a: A): Parser[A]

  /**
    * Recognizes zero or more instances of `p`
    * run(count(2, "a")("b123")) == Right(0)
    * run(count(2, "a")("a1a3")) == Right(2)
    */
  def many[A](p: Parser[A]): Parser[List[A]] =
    attempt(map2(p, many(p))(_ :: _)) | succeed(List())

  /**
    * Recognizes one or more of `p`
    */
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // TOOD: does this need to be nonstrict?

  def anyOf[A](ps: List[Parser[A]]): Parser[A] = anyOf(ps.head, ps.tail: _*)
  def anyOf[A](p: Parser[A], ps: Parser[A]*): Parser[A] =
    ps.headOption match {
      case None     => p
      case Some(p2) => attempt(p) | anyOf(p2, ps.tail: _*)
    }

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p1
      b <- p2
    } yield (a, b)

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(
      f: (A, B) => C): Parser[C] =
    product(p1, p2) map f.tupled

  def count[A](p: Parser[A]): Parser[Int] = p.many.map(_.length)

  // Implicit class for creating infix operations
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): ParserOps[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): ParserOps[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice[A]: Parser[String] = self.slice(p)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

}


trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  import Parser._

  def jsonParser: Parser[JSON] = {
    val spaces = char(' ').many.slice

    val digit = regex("[0-9]".r)
    val onenine = regex("[1-9]".r)
    val digits = many(digit)

    val hex = digit | regex("[A-Fa-f]".r)

    val number: Parser[JNumber] = regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).slice.map(s => JNumber(s.toDouble))

    val whitespace = attempt(anyOf(
      char('\u0020'),
      char('\u000D'),
      char('\u000A'),
      char('\u0009'),
    )) | succeed("")

    val escape = slice(anyOf(List('"', '\\', '/', 'b', 'f', 'n', 'r',
      't').map(char)) | char('u') ** listOfN(4, hex))

    // TODO exclude other invalid characters
    val character = regex("""[^"\\]""".r) | (char('\\') **  escape )

    def characters: Parser[Any] = (character ** characters) | succeed("")

    val jstring: Parser[JString] = for {
      _ <- char('"')
      c <- characters.slice
      _ <- char('"')
    } yield JString(c)

    val jtrue: Parser[JBool] = string("true").map(_ => JBool(true))
    val jfalse: Parser[JBool] = string("false").map(_ => JBool(false))
    val jnull: Parser[JNull.type] = string("null").map( _ => JNull)

    def value: Parser[JSON] = anyOf(array, jstring, number, jtrue, jfalse, jnull) // TODO object, array

    def element: Parser[JSON] = for {
      _ <- whitespace
      v <- value
      _ <- whitespace
      } yield v

    def elements: Parser[List[JSON]] = attempt(element).map(
      List(_)) | (element ** char(',') ** elements).map {
       case ((e, _), l) => e :: l
      }

    def array: Parser[JArray] =
      attempt((char('[') ** whitespace ** char(']')).map(_ => JArray(Nil.to[IndexedSeq]))) | (for {
          _ <- char('[')
          e <- elements
          _ <- char(']')
        } yield JArray(e.to[IndexedSeq]))

    element: Parser[JSON]
  }
}

object Main extends App {

  import Parser._

  def assertI[A](s1: A, s2: A) =
    if (s1 != s2) {
      println(s"$s1 not equal to $s2")
      assert(s1 == s2)
    } else assert(s1 == s2)

  assertI(run(char('c'))("c"), Right('c'))
  assertI(run("ab12")("ab12"), Right("ab12"))
  assertI(run(listOfN(3, "ab"))("abababab"), Right(List("ab", "ab", "ab")))
  assertI(run("c" | "b")("cb"), Right("c"))
  assertI(run("c" | "b")("bc"), Right("b"))
  assertI(run(count("c"))("ccccb"), Right(4))

  assertI(run(slice("c" ** "b"))("cbafs"), Right("cb"))

  assertI(errorMessage(run(label("failed")("c"))("input").left.get), "failed")

  val context = for {
    n <- regex("[0-9]+".r).map(_.toInt)
    _ <- listOfN(n, "a")
  } yield ()

  assertI(run(context.slice)("3aaaaaa"), Right("3aaa"))

  import JSON._

  assertI(run(jsonParser)("\"string\""), Right(JString("string")))
  assertI(run(jsonParser)("\"\""), Right(JString("")))
  assertI(run(jsonParser)("1.0"), Right(JNumber(1.0)))
  assertI(run(jsonParser)("1"), Right(JNumber(1)))
  assertI(run(jsonParser)("12"), Right(JNumber(12)))
  assertI(run(jsonParser)("10"), Right(JNumber(10)))
  assertI(run(jsonParser)("0.2"), Right(JNumber(0.2)))
  assertI(run(jsonParser)("1.2"), Right(JNumber(1.2)))
  assertI(run(jsonParser)("true"), Right(JBool(true)))
  assertI(run(jsonParser)("false"), Right(JBool(false)))
  assertI(run(jsonParser)("null"), Right(JNull))

  assertI(run(jsonParser)("[ ]"), Right(JArray(IndexedSeq.empty)))
  assertI(run(jsonParser)("[ 4, true]"), Right(JArray(IndexedSeq(JNumber(4), JBool(true)))))
//  assertI(run(jsonParser)("1.2"), Right(JNumber(1.2)))
}
