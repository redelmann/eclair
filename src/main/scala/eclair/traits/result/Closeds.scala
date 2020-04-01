package eclair
package traits
package result

import scala.collection.mutable.{ Buffer, ListBuffer }

import eclair.util._

/** Trait for enumeration of all result values.
  *
  * @group trait
  */
trait Closeds extends Results { self: Tokens with Parsers =>
  override type Result[+A] = Closed[A]

  override private[eclair] val resultBuilder: ResultBuilder = new ResultBuilder {
    override def token(token: Token): Result[Token] =
      Closed.Success(token)
    override def single[A](value: A): Result[A] =
      Closed.Success(value)
    override def union[A](left: Result[A], right: Result[A]): Result[A] =
      Closed.Disjunction(left, right)
    override def pair[A, B](left: Result[A], right: Result[B]): Result[A ~ B] =
      left ~ right
    override def map[A, B](inner: Result[A], function: A => B): Result[B] =
      inner.map(function)
    override def join[A](inner: Result[A]): (Result[A], Result[A] => Unit) = {
      val buffer = new ListBuffer[Result[A]]
      buffer += inner
      (Closed.Join(buffer), (result: Result[A]) => buffer += result)
    }
    override def rec[A](inner: Result[A] => Result[A]): Result[A] = {
      lazy val res: Result[A] = Closed.Recursive.create(inner(res))
      res
    }
  }

  /** Description of accepted values.
    *
    * @group result
    */
  sealed trait Closed[+A] {
    override def toString: String = "Closed"

    /** Returns an iterator over all accepted values. */
    def getIterator(): Iterator[A] = getProducer(Map.empty).toIterator()

    private[eclair] def ~[B](that: Closed[B]): Closed[A ~ B] = (this, that) match {
      case (Closed.Success(thisValue), Closed.Success(thatValue)) => Closed.Success(thisValue ~ thatValue)
      case _ => Closed.Sequence(this, that)
    }

    private[eclair] def map[B](function: A => B): Closed[B] = this match {
      case Closed.Success(value) => Closed.Success(function(value))
      case _ => Closed.Transform(this, function)
    }

    protected def getProducer(recs: Map[Int, () => Producer[Any]]): Producer[A]
  }

  private object Closed {
    case class Success[A](value: A) extends Closed[A] {
      override protected def getProducer(recs: Map[Int, () => Producer[Any]]): Producer[A] =
        Producer.single(value)
    }
    case class Disjunction[A](left: Closed[A], right: Closed[A]) extends Closed[A] {
      override protected def getProducer(recs: Map[Int, () => Producer[Any]]): Producer[A] =
        Producer.union(left.getProducer(recs), right.getProducer(recs))
    }
    case class Sequence[A, B](left: Closed[A], right: Closed[B]) extends Closed[A ~ B] {
      override protected def getProducer(recs: Map[Int, () => Producer[Any]]): Producer[A ~ B] =
        left.getProducer(recs) ~ right.getProducer(recs)
    }
    case class Transform[A, B](inner: Closed[A], function: A => B) extends Closed[B] {
      override protected def getProducer(recs: Map[Int, () => Producer[Any]]): Producer[B] =
        inner.getProducer(recs).map(function)
    }
    case class Join[A](inners: Buffer[Closed[A]]) extends Closed[A] {
      override protected def getProducer(recs: Map[Int, () => Producer[Any]]): Producer[A] = {
        Producer.union(inners.toSeq.map(_.getProducer(recs)): _*)
      }
    }

    sealed abstract class Recursive[A] extends Closed[A] {
      val id: Int
      def inner: Closed[A]

      override protected def getProducer(recs: Map[Int, () => Producer[Any]]): Producer[A] = recs.get(id) match {
        case Some(function) => function().asInstanceOf[Producer[A]]
        case None => {
          Producer.recursive[A] { function =>
            inner.getProducer(recs + (id -> function))
          }
        }
      }
    }

    object Recursive {
      private var nextId = 0

      def create[A](closed: => Closed[A]): Recursive[A] = {
        val newId = nextId
        nextId += 1
        new Recursive[A] {
          override val id: Int = newId
          override lazy val inner: Closed[A] = closed
        }
      }

      def unapply[A](closed: Closed[A]): Option[(Closed[A], Int)] = {
        if (closed.isInstanceOf[Recursive[_]]) {
          val casted = closed.asInstanceOf[Recursive[A]]
          Some((casted.inner, casted.id))
        }
        else {
          None
        }
      }
    }
  }
}