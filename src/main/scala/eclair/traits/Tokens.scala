package eclair
package traits

/** Contains definitions of tokens and kinds.
  *
  * @group trait
  */
trait Tokens {

  /** Type of tokens.
    *
    * @group abstract
    */
  type Token

  /** Type of token kinds.
    *
    * @group abstract
    */
  type Kind

  /** Return the unique kind of a token.
    *
    * @group abstract
    */
  def getKind(token: Token): Kind
}