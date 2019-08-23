import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util.matching.Regex


trait Parsers[Parser[+ _]] { self =>

   case class Location(input: String, offset: Int = 0) {
     lazy val line = input.slice(0, offset +1).count(_ == '\n') + 1
     lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
       case -1 => offset + 1
       case lineStart => offset - lineStart
     }
   }

   def errrorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

case class ParseError(stack: List[(Location, String)])

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
    for {
      a <- p
    } yield f(a)

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

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

  // TOOD: does this need to be nonstrict?

  def anyOf[A](ps: List[Parser[A]]): Parser[A] = anyOf(ps.head, ps.tail: _*)
  def anyOf[A](p: Parser[A], ps: Parser[A]*): Parser[A] =
    ps.headOption match {
      case None     => p
      case Some(p2) => anyOf(p2, ps.tail: _*)
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

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val spaces = char(' ').many.slice

    val jstring: Parser[JString] = for {
      _ <- char('"')
      value <- regex("""[^"]*""".r) // TODO exclude other escapes (/)
      _ <- char('"')
    } yield JString(value)

    val digit = regex("[0-9]".r)
    val onenine = regex("[1-9]".r)
    val digits = many1(digit).slice

    val fraction: Parser[Double] = ("" | ("." ** digits)).slice.map(_.toDouble)

    val sign = (succeed("") | "+" | "-").map(s => if (s == "-") -1 else 1)

    val exponent: Parser[Int] = (for {
      e <- char('e') | char('E')
      s <- sign
      d <- digits.map(_.toInt)
    } yield 10 ^ (s * d)) | succeed("").map(_ => 1)

    val hex = digit | regex("[A-Fa-f]".r)

    val escape = slice(
      anyOf(List('"', '\\', '/', 'b', 'f', 'n', 'r', 't').map(char)) | char(
        'u') ** listOfN(4, hex))

    val integer: Parser[Int] = anyOf(
      digit,
      onenine ** digits,
      "-" ** digit,
      "-" ** onenine ** digits
    ).slice.map(_.toInt)

    val number: Parser[JNumber] = for {
      i <- integer
      f <- fraction
      e <- exponent
    } yield JNumber((i + f) * e)

    val whitespace = anyOf(
      char('\u0020'),
      char('\u000D'),
      char('\u000A'),
      char('\u0009'),
    ) | succeed("")


    // TODO
   number : Parser[JSON]
  }
}


object Main extends App {

//   import ParsersImpl._

//   def assertI[A](s1: A, s2: A) =
//     if (s1 != s2) {
//       println(s"$s1 not equal to $s2")
//       assert(s1 == s2)
//     } else assert(s1 == s2)

//   assertI(run(char('c'))("c"), Right('c'))
//   assertI(run("ab12")("ab12"), Right("ab12"))
// //  assertI(run(listOfN(3, "ab"))("ababab"), Right(List("ab", "ab", "ab")))
//   // assertI(run("c" | "b")("cb"), Right("c"))
//   // assertI(run("c" | "b")("bc"), Right("b"))
//   //  assertI(run(count("c"))("ccccb"), Right("cccc"))

//   val context = for {
//     n <- regex("[0-9]+".r).map(_.toInt)
//     _ <- listOfN(n, "a")
//   } yield ()

}

// object ListHelper {

//   def unfold[A, S](z: S)(f: S => Option[(A, S)]): List[A] = {
//     f(z) match {
//       case Some((a, s)) => a :: unfold(s)(f)
//       case _            => Nil
//     }
//   }

// }

//import Parser._

// object ParsersImpl extends Parsers[ParseError, Parser] {

//   def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
//     p(input).map(_.result)
//   }

//   implicit def string(s: String): Parser[String] =
//     input =>
//       input.startsWith(s) match {
//         case true  => Right(ParseState(s, input.drop(s.length)))
//         case false => Left(ParseError(s))
//     }

// //  def slice[A](p: Parser[A]): Parser[String] = ???

//   def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
//     input =>
//       p1(input) match {
//         case Left(e) => p2(input)
//         case r       => r
//     }
// }


/**
  * Implementation
  */
// case class ParseError(msg: String)

// case class ParseState[+A](result: A, nextInput: String) {}

// object Parser {

//   type ParseResult[+A] = Either[ParseError, ParseState[A]]
//   type Parser[+A] = String => ParseResult[A]

//   def success[A](result: A, nextInput: String): ParseResult[A] =
//     Right(ParseState(result, nextInput))

//   def pure[A](result: A): Parser[A] = success(result, _)

//   def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
//     input => p(input).map(state => state.copy(result = f(state.result)))

//   def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
//     input => p(input).flatMap(state => f(state.result)(state.nextInput))

//   // Combine a list of parsers, producing a new parser
//   def fold[A, B](ps: List[Parser[A]])(base: B)(f: (B, A) => B): Parser[B] =
//     input =>
//       ps.foldLeft[ParseResult[B]](Right(ParseState(base, input)))((accum, p) =>
//         accum.flatMap(oldState =>
//           p(oldState.nextInput).map(newState =>
//             newState.copy(result = f(oldState.result, newState.result)))))

//   def takeWhile[A](p: Parser[A]): Parser[List[A]] = input => {
//     val states = ListHelper.unfold(input)(input =>
//       p(input) match {
//         case Left(e)      => None
//         case Right(state) => Some((state, state.nextInput))
//     })

//     states match {
//       case head :: tail => success(states.map(_.result).reverse, head.nextInput)
//       case _            => success(List.empty[A], input)
//     }
//   }
// }
