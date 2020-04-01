package eclair
package util

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait Peek[+A] {
  def map[B](function: A => B): Peek[B] = this match {
    case Available(value) => Available(function(value))
    case Unavailable => Unavailable
    case Terminated => Terminated
  }
}
case class Available[+A](value: A) extends Peek[A]
case object Unavailable extends Peek[Nothing]
case object Terminated extends Peek[Nothing]

trait Producer[+A] { self =>
  def peek: Peek[A]
  def skip(): Unit

  def toIterator(): Iterator[A] = new Iterator[A] {
    private var ready: Boolean = false
    private var cache: Option[A] = None

    private def prepare(): Unit = {
      if (!ready) {
        cache = self.peek match {
          case Available(value) => Some(value)
          case _ => None
        }
        ready = true
      }
    }

    override def next(): A = {
      prepare()
      val res = cache.get
      self.skip()
      cache = None
      ready = false
      res
    }
    override def hasNext: Boolean = {
      prepare()
      cache.nonEmpty
    }
  }

  def map[B](function: A => B): Producer[B] = new Producer[B] {
    private var cache: Peek[B] = Unavailable
    private def fetch(): Unit = {
      if (cache == Unavailable) {
        cache = self.peek.map(function)
      }
    }
    override def peek: Peek[B] = {
      fetch()
      cache
    }
    override def skip(): Unit = {
      fetch()
      cache = Unavailable
      self.skip()
    }
  }

  def ~[B](that: Producer[B]): Producer[A ~ B] = {
    val thisBuffer: ArrayBuffer[A] = new ArrayBuffer
    val thatBuffer: ArrayBuffer[B] = new ArrayBuffer
    var thisEnded = false
    var thatEnded = false
    var thisNext = 0
    var thatNext = 0
    var cache: Peek[A ~ B] = Unavailable

    def move(): Unit = {
      thisNext -= 1
      thatNext += 1

      def invalid = thisNext < 0 || thatNext > thatBuffer.size || (thatEnded && thatNext == thatBuffer.size)

      if (invalid) {
        val target = thisNext + thatNext + 1
        thisNext = if (thisEnded) thisBuffer.size - 1 else thisBuffer.size
        thatNext = target - thisNext

        if (invalid) {
          cache = Terminated
        }
      }
    }

    @tailrec
    def initCache(): Unit = {
      if (cache != Unavailable) {
        return
      }
      val thisPeeked =
        if (thisNext == thisBuffer.size) {
          if (thisEnded) Terminated else {
            val peeked = self.peek
            peeked match {
              case Available(value) => {
                thisBuffer += value
                self.skip()
              }
              case Terminated => thisEnded = true
              case _ => ()
            }
            peeked
          }
        }
        else {
          Available(thisBuffer(thisNext))
        }

      thisPeeked match {
        case Terminated => {
          move()
          initCache()
        }
        case Unavailable => ()
        case Available(thisValue) => {
          val thatPeeked =
            if (thatNext == thatBuffer.size) {
              if (thatEnded) Terminated else {
                val peeked = that.peek
                peeked match {
                  case Available(value) => {
                    thatBuffer += value
                    that.skip()
                  }
                  case Terminated => thatEnded = true
                  case _ => ()
                }
                peeked
              }
            }
            else {
              Available(thatBuffer(thatNext))
            }

          thatPeeked match {
            case Terminated => {
              move()
              initCache()
            }
            case Unavailable => ()
            case Available(thatValue) => {
              cache = Available(thisValue ~ thatValue)
            }
          }
        }
      }
    }


    new Producer[A ~ B] {
      override def peek: Peek[A ~ B] = {
        initCache()
        cache
      }

      override def skip() = {
        initCache()
        cache = Unavailable
        move()
      }
    }
  }
}
object Producer {
  val empty: Producer[Nothing] = new Producer[Nothing] {
    override def peek: Peek[Nothing] = Terminated
    override def skip(): Unit = ()
  }

  def single[A](value: A): Producer[A] = new Producer[A] {
    private var cache: Peek[A] = Available(value)
    override def peek: Peek[A] = cache
    override def skip(): Unit = cache = Terminated
  }

  def union[A](producers: Producer[A]*) = new Producer[A] {
    private var inners: Seq[Producer[A]] = producers
    private var cache: Peek[A] = Unavailable
    private var cachedProducer: Producer[A] = null
    private def fetch(): Unit = {
      if (cache == Unavailable) {
        var unavailables: Seq[Producer[A]] = Seq.empty

        @tailrec
        def go(): Unit = if (inners.nonEmpty) {
          val inner = inners.head
          inners = inners.tail
          inner.peek match {
            case Unavailable => {
              unavailables :+= inner
              go()
            }
            case Terminated => go()
            case available => {
              cache = available
              cachedProducer = inner
            }
          }
        }

        go()

        if (cache == Unavailable && unavailables.isEmpty) {
          cache = Terminated
        }
        else {
          inners = unavailables ++ inners
        }
      }
    }
    override def peek: Peek[A] = {
      fetch()
      cache
    }
    override def skip(): Unit = {
      fetch()
      cache = Unavailable

      cachedProducer.skip()
      inners :+= cachedProducer
      cachedProducer = null
    }
  }

  def recursive[A](inner: (() => Producer[A]) => Producer[A]): Producer[A] = {
    var terminated = false
    val buffer: ArrayBuffer[A] = new ArrayBuffer

    val createView: () => Producer[A] = () => new Producer[A] {
      private var i = 0
      override def peek: Peek[A] = {
        if (i < buffer.size) Available(buffer(i))
        else if (terminated) Terminated
        else Unavailable
      }
      override def skip(): Unit = i += 1
    }

    val applied = inner(createView)

    new Producer[A] {
      private var cache: Peek[A] = Unavailable
      private def fetch(): Unit = {
        if (cache == Unavailable) {
          cache = applied.peek
          cache match {
            case Available(value) => buffer += value
            case Terminated => terminated = true
            case Unavailable => ()
          }
        }
      }
      override def peek: Peek[A] = {
        fetch()
        cache
      }
      override def skip(): Unit = {
        fetch()
        cache = Unavailable
        applied.skip()
      }
    }
  }
}


object Prod {
  import Producer._
  def make(): Producer[Any] = {
    recursive[Any](create =>
      union(create() ~ create(), single(1), single(2))
    )
  }
}