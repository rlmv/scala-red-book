import scala.language.implicitConversions

trait Parsers[ParseError, Parser[+_]] { self =>

  def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Implicit class for creating infix operations
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): ParserOps[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): ParserOps[B] = self.or(p, p2)
  }
}


object Main extends App {

}
