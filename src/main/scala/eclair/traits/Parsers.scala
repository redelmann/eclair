package eclair
package traits

/** Contains the definition of the interface of parsers.
  *
  * @group trait
  */
trait Parsers { self: Results with Tokens =>

  /** Parser.
    *
    * @group parsing
    */
  trait Parser[+A] {

    /** Parses a sequence of tokens.
      *
      * In case parsing fails, tokens are not consumed
      * further than the point of error.
      *
      * @param tokens An iterator of tokens.
      *
      * @group parsing
      */
    def apply(tokens: Iterator[Token]): ParseResult[A]

    /** Returns the kinds of tokens that start valid sequences.
      *
      * @group property
      */
    def firstSet: Set[Kind[Any]]

    /** Checks whether `this` parser accepts the empty sequence.
      *
      * @group property
      */
    def isNullable: Boolean

    /** Checks whether `this` parser accepts at least a sequence.
      *
      * @group property
      */
    def isProductive: Boolean
  }
}