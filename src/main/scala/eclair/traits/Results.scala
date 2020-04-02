package eclair
package traits

/** Contains definition of parse results and the interface for building results.
  *
  * @group trait
  */
trait Results { self: Parsers with Tokens =>

  /** Type of result values.
    *
    * @group result
    */
  type ResultValue[+A]

  private[eclair] type Result[+A]

  private[eclair] val resultBuilder: ResultBuilder

  private[eclair] trait ResultBuilder {
    def single[A](value: A): Result[A]
    def union[A](left: Result[A], right: Result[A]): Result[A]
    def pair[A, B](left: Result[A], right: Result[B]): Result[A ~ B]
    def map[A, B](inner: Result[A], function: A => B): Result[B]
    def join[A](inner: Result[A]): (Result[A], Result[A] => Unit)
    def rec[A](inner: Result[A] => Result[A]): Result[A]
    def get[A](result: Result[A]): ResultValue[A]
  }

  /** Type of parse results.
    *
    * @group result
    */
  sealed trait ParseResult[+A] {
    def get: Option[ResultValue[A]]
    val rest: Parser[A]
  }

  /** Successful parse.
    *
    * @param values The description of parse value.
    * @param rest   Parser for more input.
    *
    * @group result
    */
  case class Parsed[+A](values: ResultValue[A], rest: Parser[A]) extends ParseResult[A] {
    override def get: Option[ResultValue[A]] = Some(values)
  }

  /** Unsuccessful parse due to an unexpected token.
    *
    * @param token The unexptected token.
    * @param rest  Parser at the point of error.
    *
    * @group result
    */
  case class UnexpectedToken[+A](token: Token, rest: Parser[A]) extends ParseResult[A] {
    override def get: Option[ResultValue[A]] = None
  }


  /** Unsuccessful parse due to an unexpected end of input.
    *
    * @param rest  Parser at the point of error.
    *
    * @group result
    */
  case class UnexpectedEnd[+A](rest: Parser[A]) extends ParseResult[A] {
    override def get: Option[ResultValue[A]] = None
  }


}