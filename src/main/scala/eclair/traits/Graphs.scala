package eclair
package traits

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.{ Buffer, ListBuffer }

import eclair.util._

/** Contains data structure definitions, as well as
  * the parsing with derivatives and zipper algorithm.
  *
  * @group trait
  */
trait Graphs { self: Results with Parsers with Tokens with Combinators =>

  import Syntax._

  /** Describes a syntax.
    *
    * @group syntax
    */
  sealed trait Syntax[+A] extends Parser[A] with Combinable[A] {
    override def apply(tokens: Iterator[Token]): ParseResult[A] =
      Alternatives(Seq(FocusedSyntax(this)), Seq.empty)(tokens)

    override def firstSet: Set[Kind[Any]] = KindSet.toSet(first)

    private[eclair] def first: KindSet

    private[eclair] def init(): Unit

    private[eclair] def nullRecs: Set[Int]

    private[eclair] final def close: Result[A] = close(Map.empty)
    protected def close(recs: Map[Int, Result[Any]]): Result[A]

    private[eclair] def registerProductive(callback: () => Unit): Unit
    private[eclair] def registerNullable(callback: () => Unit): Unit
    private[eclair] def registerFirst(callback: KindSet => Unit): Unit
    private[eclair] def registerNullRecs(callback: Set[Int] => Unit): Unit

    private[eclair] def pierce[B](signature: KindSet, context: Context[A, B], state: PierceState[B]): Unit =
      throw new IllegalStateException("Pierce is not supported on this object.")
  }

  private[eclair] object Syntax {
    trait BasicSyntax[+A] extends Syntax[A] {

      override private[eclair] def nullRecs: Set[Int] = Set.empty

      override private[eclair] def init(): Unit = ()

      override private[eclair] def registerProductive(callback: () => Unit): Unit =
        if (isProductive) callback() else ()

      override private[eclair] def registerNullable(callback: () => Unit): Unit =
        if (isNullable) callback() else ()

      override private[eclair] def registerFirst(callback: KindSet => Unit): Unit =
        if (first.nonEmpty) callback(first) else ()

      override private[eclair] def registerNullRecs(callback: Set[Int] => Unit): Unit =
        ()
    }

    trait CombinatorSyntax[+A] extends Syntax[A] {

      private var inited: Boolean = false

      override private[eclair] def init(): Unit = {
        if (!inited) {
          inited = true
          setup()
        }
      }

      protected def setup(): Unit

      private var firstSubscribers: Seq[KindSet => Unit] = Seq.empty
      private var productiveSubscribers: Seq[() => Unit] = Seq.empty
      private var nullableSubscribers: Seq[() => Unit] = Seq.empty
      private var nullRecsSubscribers: Seq[Set[Int] => Unit] = Seq.empty

      private var _first: KindSet = KindSet.empty
      private var _isProductive: Boolean = false
      private var _isNullable: Boolean = false
      private var _nullRecs: Set[Int] = Set.empty

      override private[eclair] def registerProductive(callback: () => Unit): Unit =
        if (_isProductive) callback() else productiveSubscribers +:= callback

      override private[eclair] def registerNullable(callback: () => Unit): Unit =
        if (_isNullable) callback() else nullableSubscribers +:= callback

      override private[eclair] def registerFirst(callback: KindSet => Unit): Unit = {
        firstSubscribers +:= callback
        if (_first.nonEmpty) callback(_first)
      }

      override private[eclair] def registerNullRecs(callback: Set[Int] => Unit): Unit = {
        nullRecsSubscribers +:= callback
        if (_nullRecs.nonEmpty) callback(_nullRecs)
      }

      private[eclair] def addFirst(kinds: KindSet): Unit = {
        val newKinds = kinds -- _first
        _first = _first union newKinds
        if (newKinds.nonEmpty) {
          firstSubscribers.foreach(callback => callback(newKinds))
        }
      }

      private[eclair] def addNullRec(recs: Set[Int]): Unit = {
        val newRecs = recs -- _nullRecs
        _nullRecs = _nullRecs union newRecs
        if (newRecs.nonEmpty) {
          nullRecsSubscribers.foreach(callback => callback(newRecs))
        }
      }

      private[eclair] def setProductive(): Unit = {
        _isProductive = true
        productiveSubscribers.foreach(callback => callback())
        productiveSubscribers = Seq.empty
      }

      private[eclair] def setNullable(): Unit = {
        _isNullable = true
        nullableSubscribers.foreach(callback => callback())
        nullableSubscribers = Seq.empty
      }

      override lazy val first: KindSet = {
        init()
        _first
      }
      override lazy val isProductive: Boolean = {
        init()
        _isProductive
      }
      override lazy val isNullable: Boolean = {
        init()
        _isNullable
      }
      override private[eclair] lazy val nullRecs: Set[Int] = {
        init()
        _nullRecs
      }
    }

    case class Success[+A](value: A) extends BasicSyntax[A] {
      override val first: KindSet = KindSet.empty
      override val isProductive: Boolean = true
      override val isNullable: Boolean = true

      override protected def close(recs: Map[Int, Result[Any]]): Result[A] = resultBuilder.single(value)
    }

    case object Failure extends BasicSyntax[Nothing] {
      override val first: KindSet = KindSet.empty
      override val isProductive: Boolean = false
      override val isNullable: Boolean = false

      override protected def close(recs: Map[Int, Result[Any]]): Result[Nothing] =
        throw new UnsupportedOperationException("Failure is not nullable.")
    }

    case class Elem[A](kind: Kind[A]) extends BasicSyntax[A] {
      override val first: KindSet = KindSet(kind)
      override val isProductive: Boolean = true
      override val isNullable: Boolean = false

      override protected def close(recs: Map[Int, Result[Any]]): Result[A] =
        throw new UnsupportedOperationException("Elem is not nullable.")

      override private[eclair] def pierce[B](signature: KindSet, context: Context[A, B], state: PierceState[B]): Unit = {
        state.registerResult(kind, context)
      }
    }

    case class Disjunction[+A](left: Syntax[A], right: Syntax[A]) extends CombinatorSyntax[A] {
      override protected def setup(): Unit = {
        left.init()
        right.init()

        left.registerProductive(() => setProductive())
        right.registerProductive(() => setProductive())

        left.registerNullable(() => setNullable())
        right.registerNullable(() => setNullable())

        left.registerFirst(kinds => addFirst(kinds))
        right.registerFirst(kinds => addFirst(kinds))

        left.registerNullRecs(recs => addNullRec(recs))
        right.registerNullRecs(recs => addNullRec(recs))
      }

      override protected def close(recs: Map[Int, Result[Any]]): Result[A] = {
        val inLeft = left.isNullable
        if (inLeft && right.isNullable) {
          resultBuilder.union(left.close(recs), right.close(recs))
        }
        else if (inLeft) {
          left.close(recs)
        }
        else {
          right.close(recs)
        }
      }

      override private[eclair] def pierce[B](signature: KindSet, context: Context[A, B], state: PierceState[B]): Unit = {
        val signLeft = signature & left.first
        val signRight = signature & right.first

        val inLeft = signLeft.nonEmpty

        if (inLeft && signRight.nonEmpty) {
          val join = Context.Join.create(context)
          left.pierce(signLeft, join, state)
          right.pierce(signRight, join, state)
        }
        else if (inLeft) {
          left.pierce(signLeft, context, state)
        }
        else {
          right.pierce(signRight, context, state)
        }
      }
    }

    case class Sequence[+A, +B](left: Syntax[A], right: Syntax[B]) extends CombinatorSyntax[A ~ B] {
      override protected def setup(): Unit = {
        left.init()
        right.init()

        var prodLeft = false
        var prodRight = false
        left.registerProductive(() => {
          prodLeft = true
          if (prodRight) setProductive()
        })
        right.registerProductive(() => {
          prodRight = true
          if (prodLeft) setProductive()
        })

        var nullLeft = false
        var nullRight = false
        left.registerNullable(() => {
          nullLeft = true
          if (nullRight) setNullable()
        })
        right.registerNullable(() => {
          nullRight = true
          if (nullLeft) setNullable()
        })

        var firstFromLeft: Option[KindSet] = Some(KindSet.empty)
        left.registerFirst(kinds => firstFromLeft match {
          case None => addFirst(kinds)
          case Some(oldKinds) => firstFromLeft = Some(oldKinds union kinds)
        })
        right.registerProductive(() => firstFromLeft match {
          case None => ()
          case Some(kinds) => {
            firstFromLeft = None
            addFirst(kinds)
          }
        })

        var firstFromRight: Option[KindSet] = Some(KindSet.empty)
        left.registerNullable(() => firstFromRight match {
          case None => ()
          case Some(kinds) => {
            firstFromRight = None
            addFirst(kinds)
          }
        })
        right.registerFirst(kinds => firstFromRight match {
          case None => addFirst(kinds)
          case Some(oldKinds) => firstFromRight = Some(oldKinds union kinds)
        })

        var nullRecsFromRight: Option[Set[Int]] = Some(Set.empty)
        left.registerNullable(() => nullRecsFromRight match {
          case None => ()
          case Some(recs) => {
            nullRecsFromRight = None
            addNullRec(recs)
          }
        })
        right.registerNullRecs(recs => nullRecsFromRight match {
          case None => addNullRec(recs)
          case Some(oldRecs) => nullRecsFromRight = Some(oldRecs union recs)
        })

        var nullRecsFromLeft: Option[Set[Int]] = Some(Set.empty)
        left.registerNullRecs(recs => nullRecsFromLeft match {
          case None => addNullRec(recs)
          case Some(oldRecs) => nullRecsFromLeft = Some(oldRecs union recs)
        })
        right.registerNullable(() => nullRecsFromLeft match {
          case None => ()
          case Some(recs) => {
            nullRecsFromLeft = None
            addNullRec(recs)
          }
        })
      }

      override protected def close(recs: Map[Int, Result[Any]]): Result[A ~ B] = {
        resultBuilder.pair(left.close(recs), right.close(recs))
      }

      override private[eclair] def pierce[C](signature: KindSet, context: Context[A ~ B, C], state: PierceState[C]): Unit = {
        val signLeft = signature & left.first
        val signRight = signature & right.first

        val inLeft = signLeft.nonEmpty

        if (inLeft && left.isNullable && signRight.nonEmpty) {
          val join = Context.Join.create(context)
          left.pierce(signLeft, Context.FollowBy(right, join), state)
          right.pierce(signRight, Context.Prepend(left.close, join), state)
        }
        else if (inLeft) {
          left.pierce(signLeft, Context.FollowBy(right, context), state)
        }
        else {
          right.pierce(signRight, Context.Prepend(left.close, context), state)
        }
      }
    }

    case class Transform[A, +B](inner: Syntax[A], function: A => B) extends CombinatorSyntax[B] {
      override protected def setup(): Unit = {
        inner.init()

        inner.registerProductive(() => setProductive())
        inner.registerNullable(() => setNullable())
        inner.registerFirst(kinds => addFirst(kinds))
        inner.registerNullRecs(recs => addNullRec(recs))
      }

      override protected def close(recs: Map[Int, Result[Any]]): Result[B] = {
        resultBuilder.map(inner.close(recs), function)
      }

      override private[eclair] def pierce[C](signature: KindSet, context: Context[B, C], state: PierceState[C]): Unit = {
        inner.pierce(signature, Context.Apply(function, context), state)
      }
    }

    sealed abstract class Recursive[+A] extends CombinatorSyntax[A] {
      val id: Int
      def inner: Syntax[A]

      private lazy val isNullRec: Boolean = {
        init()
        nullRecs.contains(id)
      }

      override protected def close(recs: Map[Int, Result[Any]]): Result[A] = {
        if (recs.contains(id)) {
          recs(id).asInstanceOf[Result[A]]
        }
        else if (isNullRec) {
          resultBuilder.rec((rec: Result[A]) => inner.close(recs + (id -> rec)))
        }
        else {
          inner.close(recs)
        }
      }

      override protected def setup(): Unit = {
        inner.init()

        inner.registerProductive(() => setProductive())
        inner.registerNullable(() => setNullable())
        inner.registerFirst(kinds => addFirst(kinds))
        inner.registerNullRecs(recs => addNullRec(recs))
      }

      override private[eclair] def registerNullRecs(callback: Set[Int] => Unit): Unit = {
        super.registerNullRecs(callback)
        this.registerNullable(() => callback(Set(id)))
      }

      override private[eclair] def pierce[B](signature: KindSet, context: Context[A, B], state: PierceState[B]): Unit = {
        state.get(this) match {
          case Some(buffer) => buffer += context
          case None => {
            val buffer: Buffer[Context[A, B]] = new ListBuffer
            state.set(this, buffer)
            val shared = Context.Share(context, buffer)
            inner.pierce(signature, shared, state)
          }
        }
      }
    }

    object Recursive {
      private var nextId = 0

      def create[A](syntax: => Syntax[A]): Recursive[A] = {
        val newId = nextId
        nextId += 1
        new Recursive[A] {
          override val id: Int = newId
          override lazy val inner: Syntax[A] = {
            init()
            syntax
          }
        }
      }

      def unapply[A](syntax: Syntax[A]): Option[(Syntax[A], Int)] = syntax match {
        case s: Recursive[A] => {
          s.init()
          Some((s.inner, s.id))
        }
        case _ => None
      }
    }
  }

  private class PlugState[A] {
    private var stops: Queue[FocusedSyntax[A]] = Queue.empty
    private var dones: Queue[Result[A]] = Queue.empty
    private var nexts: Queue[FocusedResult[A]] = Queue.empty
    private var joinEntries: Map[Int, Any] = Map.empty

    def registerStop(stop: FocusedSyntax[A]): Unit = stops :+= stop
    def registerDone(done: Result[A]): Unit = dones :+= done
    def registerNext(next: FocusedResult[A]) = nexts :+= next
    def retrieveDones(): Seq[Result[A]] = {
      val res = dones
      dones = Queue.empty
      res
    }
    def retrieveNexts(): Seq[FocusedResult[A]] = {
      val res = nexts
      nexts = Queue.empty
      res
    }
    def retrieveStops(): Seq[FocusedSyntax[A]] = {
      val res = stops
      stops = Queue.empty
      res
    }
    def get[B](join: Context.Join[B, A]): Option[Result[B] => Unit] =
      joinEntries.get(join.id).map(_.asInstanceOf[Result[B] => Unit])
    def set[B](join: Context.Join[B, A], adder: Result[B] => Unit): Unit =
      joinEntries += join.id -> adder
  }

  private class PierceState[A](private val mappings: KindMappings) {
    private var results: Queue[FocusedResult[A]] = Queue.empty
    private var recEntries: Map[Int, Any] = Map.empty
    def registerResult[B](kind: Kind[B], context: Context[B, A]): Unit = {
      val value: B = KindMappings.get(mappings, kind)
      results :+= FocusedResult.Pair(resultBuilder.single(value), context)
    }
    def retrieveResults(): Seq[FocusedResult[A]] = {
      val res = results
      results = Queue.empty
      res
    }
    def get[B](rec: Recursive[B]): Option[Buffer[Context[B, A]]] =
      recEntries.get(rec.id).map(_.asInstanceOf[Buffer[Context[B, A]]])
    def set[B](rec: Recursive[B], buffer: Buffer[Context[B, A]]) =
      recEntries += rec.id -> buffer
  }

  private class LocateState[A] {
    private var results: Seq[FocusedSyntax[A]] = Seq.empty
    def registerLocated(focused: FocusedSyntax[A]): Unit = results +:= focused
    def retrieveLocateds(): Seq[FocusedSyntax[A]] = {
      val res = results
      results = Seq.empty
      res
    }
  }

  private[eclair] sealed trait FocusedSyntax[A] {
    def pierce(signature: KindSet, state: PierceState[A]): Unit
    def close: FocusedResult[A]
    def first: KindSet
  }
  private[eclair] object FocusedSyntax {
    def apply[A](syntax: Syntax[A]): FocusedSyntax[A] = Pair(syntax, Context.Empty[A]())
    case class Pair[A, B](focus: Syntax[A], context: Context[A, B]) extends FocusedSyntax[B] {
      override def pierce(signature: KindSet, state: PierceState[B]): Unit =
        focus.pierce(signature, context, state)
      override def close: FocusedResult[B] =
        FocusedResult.Pair(focus.close, context)
      override lazy val first: KindSet =
        if (focus.isNullable) focus.first union context.first else focus.first
    }
  }

  private[eclair] sealed trait FocusedResult[A] {
    def plug(state: PlugState[A]): Unit
  }
  private[eclair] object FocusedResult {
    case class Pair[A, B](focus: Result[A], context: Context[A, B]) extends FocusedResult[B] {
      override def plug(state: PlugState[B]): Unit =
        Context.plug(focus, context, state)
    }
  }

  private[eclair] sealed trait Context[-A, B] {
    def apply(syntax: Syntax[A]): FocusedSyntax[B] = FocusedSyntax.Pair(syntax, this)
    def apply(result: Result[A]): FocusedResult[B] = FocusedResult.Pair(result, this)
    def init(): Unit
    def registerFirst(callback: KindSet => Unit): Unit
    def plugStep(result: Result[A], state: PlugState[B]): Option[FocusedResult[B]]
    def first: KindSet
  }

  private[eclair] sealed trait LayerContext[A, B] extends Context[A, B] {

    private var inited: Boolean = false
    private var stable: Boolean = false

    override def init(): Unit = {
      if (!inited) {
        inited = true
        setup()
        stable = true
        firstSubscribers = Seq.empty
      }
    }

    protected def setup(): Unit

    private var firstSubscribers: Seq[KindSet => Unit] = Seq.empty
    private var _first: KindSet = KindSet.empty

    override def registerFirst(callback: KindSet => Unit): Unit = {
      if (!stable) {
        firstSubscribers +:= callback
      }
      if (_first.nonEmpty) {
        callback(_first)
      }
    }

    private[eclair] def addFirst(kinds: KindSet): Unit = {
      val newKinds = kinds -- _first
      _first = _first union newKinds
      if (newKinds.nonEmpty) {
        firstSubscribers.foreach(callback => callback(newKinds))
      }
    }

    override lazy val first: KindSet = {
      init()
      _first
    }
  }

  private[eclair] object Context {
    case class Empty[A]() extends Context[A, A] {
      override def registerFirst(callback: KindSet => Unit): Unit = ()
      override def init(): Unit = ()
      override val first: KindSet = KindSet.empty
      override def plugStep(result: Result[A], state: PlugState[A]): Option[FocusedResult[A]] = {
        state.registerDone(result)
        None
      }
    }
    case class Prepend[A, B, C](value: Result[C], tail: Context[C ~ A, B]) extends LayerContext[A, B] {
      override protected def setup(): Unit = {
        tail.init()
        tail.registerFirst(kinds => addFirst(kinds))
      }
      override def plugStep(result: Result[A], state: PlugState[B]): Option[FocusedResult[B]] = {
        Some(FocusedResult.Pair(resultBuilder.pair(value, result), tail))
      }
    }
    case class FollowBy[A, B, C](syntax: Syntax[C], tail: Context[A ~ C, B]) extends LayerContext[A, B] {
      override protected def setup(): Unit = {
        tail.init()
        addFirst(syntax.first)
        if (syntax.isNullable) {
          tail.registerFirst(kinds => addFirst(kinds))
        }
      }
      override def plugStep(result: Result[A], state: PlugState[B]): Option[FocusedResult[B]] = {
        state.registerStop(FocusedSyntax.Pair(syntax, Prepend(result, tail)))
        None
      }
    }
    case class Apply[A, B, C](function: A => C, tail: Context[C, B]) extends LayerContext[A, B] {
      override protected def setup(): Unit = {
        tail.init()
        tail.registerFirst(kinds => addFirst(kinds))
      }
      override def plugStep(result: Result[A], state: PlugState[B]): Option[FocusedResult[B]] = {
        Some(FocusedResult.Pair(resultBuilder.map(result, function), tail))
      }
    }
    case class Join[A, B] private(id: Int, tail: Context[A, B]) extends LayerContext[A, B] {
      override protected def setup(): Unit = {
        tail.init()
        tail.registerFirst(kinds => addFirst(kinds))
      }
      override def plugStep(result: Result[A], state: PlugState[B]): Option[FocusedResult[B]] = state.get(this) match {
        case Some(adder) => {
          adder(result)
          None
        }
        case None => {
          val (joinable, adder) = resultBuilder.join(result)
          state.set(this, adder)
          Some(FocusedResult.Pair(joinable, tail))
        }
      }
    }
    object Join {
      private var nextId = 0

      def create[A, B](tail: Context[A, B]): Join[A, B] = {
        val newId = nextId
        nextId += 1
        Join(newId, tail)
      }
    }
    case class Share[A, B](tail: Context[A, B], rest: Buffer[Context[A, B]]) extends LayerContext[A, B] {
      override def toString: String = "Shared"
      override protected def setup(): Unit = {
        tail.init()
        tail.registerFirst(kinds => addFirst(kinds))
        rest.foreach { context =>
          context.init()
          context.registerFirst(kinds => addFirst(kinds))
        }
      }
      override def plugStep(result: Result[A], state: PlugState[B]): Option[FocusedResult[B]] = {
        rest.foreach { context: Context[A, B] =>
          state.registerNext(FocusedResult.Pair(result, context))
        }
        Some(FocusedResult.Pair(result, tail))
      }
    }

    @tailrec
    def plug[A, B](result: Result[A], context: Context[A, B], state: PlugState[B]): Unit =
      context.plugStep(result, state) match {
        case None => ()
        case Some(FocusedResult.Pair(result, context)) => plug(result, context, state)
      }
  }

  private[eclair] case class Alternatives[A](actives: Seq[FocusedSyntax[A]], results: Seq[Result[A]]) extends Parser[A] {

    override def apply(tokens: Iterator[Token]): ParseResult[A] = {
      var currents: Seq[FocusedSyntax[A]] = actives
      var currentResults: Seq[Result[A]] = results

      while (tokens.hasNext) {
        val token = tokens.next()
        var firsts = KindSet.empty
        for (current <- currents) {
          firsts = firsts union current.first
        }
        val (signature, mappings) = KindSet.get(firsts, token)

        // Locating
        val locateds = {
          val plugState = new PlugState[A]
          val locateState = new LocateState[A]

          @inline
          def locate(focused: FocusedSyntax[A]): Unit = focused match {
            case FocusedSyntax.Pair(syntax, context) => {
              if ((syntax.first & signature).nonEmpty) {
                locateState.registerLocated(focused)
              }
              if (syntax.isNullable && (context.first & signature).nonEmpty) {
                focused.close.plug(plugState)
              }
            }
          }

          for (current <- currents) {
            locate(current)
          }

          var changed = true
          while (changed) {
            changed = false

            for (next <- plugState.retrieveNexts()) {
              changed = true
              next.plug(plugState)
            }

            for (stop <- plugState.retrieveStops()) {
              changed = true
              locate(stop)
            }
          }

          locateState.retrieveLocateds()
        }

        if (locateds.isEmpty) {
          return UnexpectedToken(token, Alternatives(currents, currentResults))
        }

        // Piercing
        val pierceds = {
          val pierceState = new PierceState[A](mappings)
          for (located <- locateds) {
            located.pierce(signature, pierceState)
          }

          pierceState.retrieveResults()
        }

        val plugState = new PlugState[A]

        for (pierced <- pierceds) {
          pierced.plug(plugState)
        }

        var changed = true
        while (changed) {
          changed = false

          for (next <- plugState.retrieveNexts()) {
            changed = true
            next.plug(plugState)
          }
        }

        currents = plugState.retrieveStops()
        currentResults = plugState.retrieveDones()
      }

      // Computing new state.
      val newState = Alternatives(currents, currentResults)

      newState.result match {
        case Some(result) => Parsed(resultBuilder.get(result), newState)
        case None => UnexpectedEnd(newState)
      }
    }

    private lazy val result: Option[Result[A]] = {
      val plugState = new PlugState[A]

      @inline
      def close(focused: FocusedSyntax[A]): Unit = focused match {
        case FocusedSyntax.Pair(syntax, context) => {
          if (syntax.isNullable) {
            focused.close.plug(plugState)
          }
        }
      }

      for (active <- actives) {
        close(active)
      }

      var changed = true
      while (changed) {
        changed = false

        for (next <- plugState.retrieveNexts()) {
          changed = true
          next.plug(plugState)
        }

        for (stop <- plugState.retrieveStops()) {
          changed = true
          close(stop)
        }
      }

      val allResults = (plugState.retrieveDones() ++ results)

      if (allResults.isEmpty) {
        None
      }
      else {
        Some(allResults.reduceLeft {
          (x, y) => resultBuilder.union(x, y)
        })
      }
    }

    override lazy val firstSet: Set[Kind[Any]] = KindSet.toSet(actives.foldLeft(KindSet.empty) {
      case (acc, focused) => acc union focused.first
    })
    override def isNullable: Boolean = result.nonEmpty
    override def isProductive: Boolean = isNullable || firstSet.nonEmpty
  }
}