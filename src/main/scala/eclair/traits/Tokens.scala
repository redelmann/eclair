package eclair
package traits

import scala.collection.immutable.{ BitSet, IntMap }
import scala.collection.mutable.{ ArrayBuffer }

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

  /** Partial function from single tokens to values of type `A`.
    *
    * Kinds are the basic unit of parsing.
    *
    * @group kind
    */
  sealed trait Kind[+A] {

    /** Checks if a token if part of the kind, and if so returns the associated value. */
    def apply(token: Token): Option[A]

    private[eclair] val id: Int

    override def hashCode(): Int = id

    override def equals(that: Any): Boolean = {
      if (that.isInstanceOf[Kind[_]]) {
        id == that.asInstanceOf[Kind[_]].id
      }
      else {
        false
      }
    }
  }

  /** Constructs a kind that describes tokens satisfying a `predicate`.
    *
    * @param predicate The predicate that tokens must satisfy to be included in the kind.
    *
    * @group kind
    */
  def acceptWhen(predicate: Token => Boolean): Kind[Token] = Kind.make {
    case token if predicate(token) => token
  }

  /** Constructs a kind that describes tokens in the domain of a partial `function`.
    *
    * @param function The partial function that specifies if tokens are to be included.
    *
    * @group kind
    */
  def acceptWith[A](function: PartialFunction[Token, A]): Kind[A] = Kind.make(function)

  private[eclair] object Kind {

    private val kinds: ArrayBuffer[Kind[Any]] = new ArrayBuffer

    def make[A](function: PartialFunction[Token, A]): Kind[A] = {
      val nextId = kinds.size

      val kind = new Kind[A] {
        override private[eclair] val id = nextId
        override def apply(token: Token): Option[A] = function.lift(token)
      }

      kinds += kind

      kind
    }

    def getKind(index: Int): Kind[Any] = kinds(index)
  }

  private[eclair] type KindSet = BitSet

  private[eclair] object KindSet {

    @inline def apply(kind: Kind[Any]): KindSet = BitSet(kind.id)

    @inline def empty: KindSet = BitSet.empty

    @inline def get(set: KindSet, token: Token): (KindSet, KindMappings) = {
      set.foldLeft((BitSet.empty, IntMap.empty[Any])) {
        case (acc@(accSet, accMap), index) => {
          val kind = Kind.getKind(index)
          kind(token) match {
            case None => acc
            case Some(res) => (accSet + index, accMap + (index -> res))
          }
        }
      }
    }

    @inline def toSet(kindSet: KindSet): Set[Kind[Any]] = {
      kindSet.unsorted.map(index => Kind.getKind(index))
    }
  }

  private[eclair] type KindMappings = IntMap[Any]

  private[eclair] object KindMappings {
    @inline def get[A](mappings: KindMappings, kind: Kind[A]): A = mappings(kind.id).asInstanceOf[A]
  }
}