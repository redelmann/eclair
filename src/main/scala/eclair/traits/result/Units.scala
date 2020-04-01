package eclair
package traits
package result

/** Trait for ignoring result values.
  *
  * @group trait
  */
trait Units extends Results { self: Tokens with Parsers =>
  override type Result[+A] = Unit

  override private[eclair] val resultBuilder: ResultBuilder = new ResultBuilder {
    override def token(token: Token): Result[Token] =
      ()
    override def single[A](value: A): Result[A] =
      ()
    override def union[A](left: Result[A], right: Result[A]): Result[A] =
      ()
    override def pair[A, B](left: Result[A], right: Result[B]): Result[A ~ B] =
      ()
    override def map[A, B](inner: Result[A], function: A => B): Result[B] =
      ()
    override def join[A](inner: Result[A]): (Result[A], Result[A] => Unit) =
      ((), (_: Result[A]) => ())
    override def rec[A](inner: Result[A] => Result[A]): Result[A] =
      ()
  }
}