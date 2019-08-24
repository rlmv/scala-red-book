import scala.language.implicitConversions
import scala.language.higherKinds

import scala.util.matching.Regex

import MyParser._


sealed trait Result[+A]
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]

// run is a function that takes input, returns A and next chunk of input
class MyParser[+A] (val run: Parser[A]) {}

object MyParser extends Parsers[MyParser] {
  type Parser[+A] = Location => Result[A]

  def run[A](p: MyParser[A])(input: String): Either[ParseError, A] =
    p.run(Location(input)) match {
      case Success(get, _) => Right(get)
      case Failure(get) => Left(get)
    }

  def succeed[A](a: A): MyParser[A]  = new MyParser(l =>
    Success(a, 0)
  )

  implicit def string(s: String): MyParser[String] = new MyParser((l: Location) =>
    if (l.remainder.startsWith(s)) Success(s, s.length)
    else Failure(l.toError(s"Expected: $s"))
  )

  implicit def regex(r: Regex): MyParser[String] = new MyParser((l: Location) =>
    r.findFirstIn(l.remainder) match {
      case Some(s) => Success(s, s.length)
      case None => Failure(l.toError(s"No match for: $r"))
      }
  )

  def slice[A](p: MyParser[A]): MyParser[String] = new MyParser((l: Location) =>
    p.run(l) match {
      case Success(_, i) => Success(l.input.slice(l.offset, l.offset + i), i)
      case Failure(get) => Failure(get)
    })

  def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = ???
  def attempt[A](p: MyParser[A]): MyParser[A] = ???

  def errorMessage(e: MyParser.ParseError): String = ???
  def errrorLocation(e: MyParser.ParseError): MyParser.Location = ???

  // ????
  def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = new MyParser(l => {
    p.run(l) match {
      case Success(get, i) => {
        println(get, l, i)
        println(l.copy(offset=l.offset + i))
        f(get).run(l.copy(offset=l.offset + i))
      }
      case Failure(get) => Failure(get)
    }})

  def label[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

  def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = new MyParser(l =>
    // TODO backtrack / pick error message
    p1.run(l) match {
      case Failure(get) => p2.run(l)
      case success => success
    }
  )

  //   if (l.input.startsWith(s)) Right((s, l.input.slice(s.length, l.input.length)))
  //   else {
  //     val i = l.input.zip(s).zipWithIndex.collectFirst {
  //       case ((x, y), i) if x != y => i
  //     } match {
  //       case Some(i) => i
  //       case None => if (s.length > l.input.length) l.input.length + 1 else s.length + 1
  //     }
  //     Left(Location(l.input, i).toError("cannot match string"))
  //   }
  // )


}

trait Parsers[Parser[+ _]] { self =>

  case class Location(input: String, offset: Int = 0) {
     lazy val line = input.slice(0, offset +1).count(_ == '\n') + 1
     lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
       case -1 => offset + 1
       case lineStart => offset - lineStart
     }

    def toError(msg: String): ParseError = ParseError(
      List((this, msg))
    )

    def remainder: String = input.slice(offset, input.length)
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
    p.flatMap(a => succeed(f(a)))

  def succeed[A](a: A): Parser[A]

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
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

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

  import MyParser._

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

  val context = for {
    n <- regex("[0-9]+".r).map(_.toInt)
    _ <- listOfN(n, "a")
  } yield ()

  assertI(run(context)("3aaa"), Right("3aaa"))

}
