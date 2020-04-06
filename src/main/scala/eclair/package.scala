
import eclair.traits.result._

/** Éclair is a parsing combinator library, implementing parsing with derivatives and zippers.
  *
  * @groupprio abstract 0
  * @groupname abstract Abstract Members
  *
  * @groupprio library 1
  * @groupname library Libraries
  *
  * @groupprio eclair 2
  * @groupname eclair Traits
  *
  * @groupprio parsing 3
  * @groupname parsing Parsing
  *
  * @groupprio result 4
  * @groupname result Results
  *
  * @groupprio kind 5
  * @groupname kind Token Kinds
  *
  * @groupprio syntax 6
  * @groupname syntax Syntaxes
  *
  * @groupprio property 7
  * @groupname property Properties
  *
  * @groupprio combinator 8
  * @groupname combinator Combinators
  *
  * @groupprio hide 9
  * @groupname hide Hiding Values
  *
  * @groupprio trait 10
  * @groupname trait Mixed-in Traits
  *
  * @groupprio visualisation 11
  * @groupname visualisation Visualisation
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

  /** Builds an Éclair library specialized to the token type `T`,
    * which you can then import from.
    *
    * {{{
    * val eclairLibrary = eclair.library[MyToken]
    * import eclairLibrary._
    * }}}
    *
    * Successful parses will return a single value.
    *
    * @group library
    */
  def library[T]: Eclair with ResultSingle { type Token = T } = new EclairSingle {
    override type Token = T
  }

  /** Builds an Éclair library specialized to the token type `T`,
    * which you can then import from.
    *
    * {{{
    * val eclairLibrary = eclair.libraryUnit[MyToken]
    * import eclairLibrary._
    * }}}
    *
    * Successful parses will not return values.
    *
    * @group library
    */
  def libraryUnit[T]: Eclair with ResultUnit { type Token = T } = new EclairUnit {
    override type Token = T
  }

  /** Builds an Éclair library specialized to the token type `T`,
    * which you can then import from.
    *
    * {{{
    * val eclairLibrary = eclair.libraryAll[MyToken]
    * import eclairLibrary._
    * }}}
    *
    * Successful parses will return all parse values.
    *
    * @group library
    */
  def libraryAll[T]: Eclair with ResultAll { type Token = T } = new EclairAll {
    override type Token = T
  }
}