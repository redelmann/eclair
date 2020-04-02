package eclair
package traits
package result

import scala.collection.mutable.ListBuffer

/** Trait for production of single result values.
  *
  * @group trait
  */
trait ResultSingle extends Results { self: Tokens with Parsers =>

  override type ResultValue[+A] = A

  override private[eclair] type Result[+A] = Single[A]

  /** Description of an accepted value.
    *
    * @group result
    */
  private[eclair] trait Single[+A]{

    /** Returns the accepted value. */
    def get: A

    private[ResultSingle] def register(callback: A => Unit): Unit
  }

  private class SingleJoin[A] extends Single[A] {
    private var state: Either[A, ListBuffer[A => Unit]] = Right(new ListBuffer)

    override def get: A = state match {
      case Left(value) => value
      case Right(_) => throw new IllegalStateException("SingleJoin not set.")
    }

    override private[ResultSingle] def register(callback: A => Unit): Unit = state match {
      case Left(value) => callback(value)
      case Right(buffer) => buffer += callback
    }

    def inform(value: A): Unit = state match {
      case Left(_) => ()
      case Right(buffer) => {
        state = Left(value)
        buffer.foreach(callback => callback(value))
      }
    }
  }

  private class SingleMerge[A, B] extends Single[A ~ B] {

    private sealed trait State
    private case class HasBoth(value: A ~ B) extends State
    private case class HasLeft(value: A, buffer: ListBuffer[(A ~ B) => Unit]) extends State
    private case class HasRight(value: B, buffer: ListBuffer[(A ~ B) => Unit]) extends State
    private case class HasNone(buffer: ListBuffer[(A ~ B) => Unit]) extends State

    private var state: State = HasNone(new ListBuffer)

    override def get: A ~ B = state match {
      case HasBoth(value) => value
      case _ => throw new IllegalStateException("SingleMerge not set.")
    }

    override private[ResultSingle] def register(callback: (A ~ B) => Unit): Unit = state match {
      case HasBoth(value) => callback(value)
      case HasLeft(_, buffer) => buffer += callback
      case HasRight(_, buffer) => buffer += callback
      case HasNone(buffer) => buffer += callback
    }

    def informLeft(leftValue: A): Unit = state match {
      case HasRight(rightValue, buffer) => {
        val value = leftValue ~ rightValue
        state = HasBoth(value)
        buffer.foreach(callback => callback(value))
      }
      case HasNone(buffer) => {
        state = HasLeft(leftValue, buffer)
      }
      case _ => ()
    }

    def informRight(rightValue: B): Unit = state match {
      case HasLeft(leftValue, buffer) => {
        val value = leftValue ~ rightValue
        state = HasBoth(value)
        buffer.foreach(callback => callback(value))
      }
      case HasNone(buffer) => {
        state = HasRight(rightValue, buffer)
      }
      case _ => ()
    }
  }

  private case class SingleDirect[A](get: A) extends Single[A] {

    override private[ResultSingle] def register(callback: A => Unit): Unit =
      callback(get)
  }

  override private[eclair] val resultBuilder: ResultBuilder = new ResultBuilder {
    override def single[A](value: A): Result[A] =
      SingleDirect(value)
    override def union[A](left: Result[A], right: Result[A]): Result[A] = {
      val result = new SingleJoin[A]
      left.register((value: A) => result.inform(value))
      right.register((value: A) => result.inform(value))
      result
    }
    override def pair[A, B](left: Result[A], right: Result[B]): Result[A ~ B] = {
      val result = new SingleMerge[A, B]
      left.register((value: A) => result.informLeft(value))
      right.register((value: B) => result.informRight(value))
      result
    }
    override def map[A, B](inner: Result[A], function: A => B): Result[B] = {
      val result = new SingleJoin[B]
      inner.register((value: A) => result.inform(function(value)))
      result
    }
    override def join[A](inner: Result[A]): (Result[A], Result[A] => Unit) = {
      val result = new SingleJoin[A]
      inner.register((value: A) => result.inform(value))
      (result, (other: Result[A]) => inner.register((value: A) => result.inform(value)))
    }
    override def rec[A](compute: Result[A] => Result[A]): Result[A] = {
      val result = new SingleJoin[A]
      val inner = compute(result)
      inner.register((value: A) => result.inform(value))
      result
    }
    override def get[A](result: Result[A]): ResultValue[A] =
      result.get
  }
}