
/** Éclair is a parsing combinator library, implementing parsing with derivatives and zippers.
  *
  * @groupprio abstract 0
  * @groupname abstract Abstract Members
  *
  * @groupprio eclair 1
  * @groupname eclair Éclair
  *
  * @groupprio parsing 2
  * @groupname parsing Parsing
  *
  * @groupprio result 3
  * @groupname result Results
  *
  * @groupprio syntax 4
  * @groupname syntax Syntaxes
  *
  * @groupprio combinator 5
  * @groupname combinator Combinators
  *
  * @groupprio hide 6
  * @groupname hide Hiding Values
  *
  * @groupprio trait 10
  * @groupname trait Mixed-in Traits
  *
  * @groupprio pairs 100
  * @groupname pairs Pairs
  */
package object eclair {

  /** Simply a pair.
    *
    * Can be used in infix position in pattern matching.
    *
    * @param _1 First element.
    * @param _2 Second element.
    * @group pairs
    */
  case class ~[+A, +B](_1: A, _2: B) {

    /* Builds a pair. */
    def ~[C](next: C): (A ~ B) ~ C = eclair.~(this, next)
  }

  /** Adds an `~` methods to build pairs.
    *
    * @group pairs
    */
  implicit class PairDecorator[A](first: A) {

    /** Builds a pair. */
    def ~[B](second: B): A ~ B = eclair.~(first, second)
  }
}