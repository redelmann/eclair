package eclair
package traits

import scala.language.implicitConversions

/** Contains combinators for building syntaxes.
  *
  * @group trait
  */
trait Combinators { self: Tokens with Graphs =>

  /** Contains combinator methods of [[eclair.Eclair#Syntax]].
    *
    * @group trait
    */
  trait Combinable[+A] { self: Syntax[A] =>

    /** For performance, indicates that `this` syntax can be accessed
      * from multiple points.
      *
      * @group combinator
      */
    lazy val shared: Syntax[A] = this match {
      case r: Syntax.Recursive[A] => r
      case _ => Syntax.Recursive.create(this)
    }

    /** Applies a function to the parsed values.
      *
      * @group combinator
      */
    def map[B](function: A => B): Syntax[B] = Syntax.Transform(this, function)

    /** Disjunction of `this` and `that` syntax.
      *
      * @group combinator
      */
    def |[B >: A](that: Syntax[B]): Syntax[B] = Syntax.Disjunction(this, that)

    /** Tagged disjunction of `this` and `that` syntax.
      *
      * @group combinator
      */
    def ||[B](that: Syntax[B]): Syntax[Either[A, B]] = this.map(Left(_)) | that.map(Right(_))

    /** Sequence of `this` and `that` syntax.
      *
      * Values are paired.
      *
      * @group combinator
      */
    def ~[B](that: Syntax[B]): Syntax[A ~ B] = Syntax.Sequence(this, that)

    /** Sequence of `this` and `that` syntax.
      *
      * Values from `that` are ignored.
      *
      * @group combinator
      */
    def ~(that: Hide): Syntax[A] = this ~<~ that.syntax

    /** Sequence of `this` and `that` syntax.
      *
      * Values from `that` are ignored.
      *
      * @group combinator
      */
    def ~<~[B](that: Syntax[B]): Syntax[A] = (this ~ that).map(_._1)

    /** Sequence of `this` and `that` syntax.
      *
      * Values from `this` are ignored.
      *
      * @group combinator
      */
    def ~>~[B](that: Syntax[B]): Syntax[B] = (this ~ that).map(_._2)

    /** Makes `this` syntax optional.
      *
      * @group combinator
      */
    def opt: Syntax[Option[A]] = this.map(Some(_)) | Syntax.Success(None)

    /** Makes `this` syntax optional.
      *
      * @param value The value to produce when `this` syntax is skipped.
      *
      * @group combinator
      */
    def opt[B >: A](value: B): Syntax[B] = this | Syntax.Success(value)

    /** Overrides the parsed values with a constant `value`.
      *
      * @param value The value to produce.
      *
      * @group combinator
      */
    def const[B](value: B): Syntax[B] = this.map(_ => value)

    /** Indicates that parsed values produced by `this` syntax
      * should be ignored when building up sequences using the `~` combinator.
      *
      * @group combinator
      */
    def hide: Hide = Hide(this)

    /** Repetition of `this` syntax 0 or more times.
      *
      * Values are aggregated from left to right.
      *
      * @param first  The initial accumulator.
      * @param append The function to append values produced by `this` to the accumulator.
      *
      * @group combinator
      */
    def repeatLeft[B](first: B)(append: (B, A) => B): Syntax[B] = {
      lazy val res: Syntax[B] = recursive {
        (res ~ this.shared).map {
          case acc ~ x => append(acc, x)
        }.opt(first)
      }

      res
    }

    /** Repetition of `this` syntax 0 or more times.
      *
      * Values are aggregated from right to left.
      *
      * @param last    The initial accumulator.
      * @param prepend The function to prepend values produced by `this` to the accumulator.
      *
      * @group combinator
      */
    def repeatRight[B](last: B)(prepend: (A, B) => B): Syntax[B] = {
      lazy val res: Syntax[B] = recursive {
        (this ~ res).map {
          case x ~ acc => prepend(x, acc)
        }.opt(last)
      }

      res
    }

    /** Repetition of `this` syntax separated by an infix binary `operator`.
      *
      * Values are aggregated from left to right.
      *
      * @param operator The syntax for the binary operator.
      *
      * @group combinator
      */
    def leftAssoc[B >: A](operator: Syntax[(B, B) => B]): Syntax[B] =
      infixLeft(this: Syntax[B])(operator)

    /** Repetition of `this` syntax separated by an infix binary `operator`.
      *
      * Values are aggregated from right to left.
      *
      * @param operator The syntax for the binary operator.
      *
      * @group combinator
      */
    def rightAssoc[B >: A](operator: Syntax[(B, B) => B]): Syntax[B] =
      infixRight(this: Syntax[B])(operator)

    /** Repetition of `this` syntax exactly `n` times.
      *
      * @param n Number of repetitions.
      *
      * @group combinator
      */
    def times(n: Int): Syntax[Vector[A]] =
      if (n < 0) {
        failure
      }
      else if (n == 0) {
        success(Vector())
      }
      else {
        (times(n - 1) ~ this).map {
          case xs ~ x => xs :+ x
        }
      }

    /** Repetition of `this` syntax between 0 and `n` times.
      *
      * @param n Maximum number of repetitions.
      *
      * @group combinator
      */
    def atMost(n: Int): Syntax[Vector[A]] =
      if (n < 0) {
        failure
      }
      else if (n == 0) {
        success(Vector())
      }
      else {
        (atMost(n - 1) ~ this.shared).map {
          case xs ~ x => xs :+ x
        }.opt(Vector())
      }

    /** Repetition of `this` syntax at least `n` times.
      *
      * @param n Minimum number of repetitions.
      *
      * @group combinator
      */
    def atLeast(n: Int): Syntax[Vector[A]] =
      (this.times(n) ~ many(this)).map {
        case xs ~ ys => xs ++ ys
      }

    /** Repetition of `this` syntax between `min` and `max` times.
      *
      * @param min Minimum number of repetitions.
      * @param max Maximum number of repetitions.
      *
      * @group combinator
      */
    def range(min: Int, max: Int): Syntax[Vector[A]] =
      (this.times(min) ~ this.atMost(max - min)).map {
        case xs ~ ys => xs ++ ys
      }
  }

  /** Indicates that the parsed values of a `syntax` should be ignored
    * when building up sequences using the `~` combinator.
    *
    * @group hide
    */
  case class Hide(syntax: Syntax[Any]) {

    /** Sequence of `this` and `that` syntax.
      *
      * Values from `this` are ignored.
      *
      * @group combinator
      */
    def ~[B](that: Syntax[B]): Syntax[B] = syntax ~>~ that

    /** Sequence of `this` and `that` syntax.
      *
      * Both values are ignored.
      *
      * @group combinator
      */
    def ~(that: Hide): Hide = Hide(this.syntax ~ that.syntax)
  }

  /** Syntax that doesn't describe any input.
    *
    * @group combinator
    */
  val failure: Syntax[Nothing] =
    Syntax.Failure

  /** Syntax that only describes the empty string.
    *
    * @param value The value to produce.
    *
    * @group combinator
    */
  def success[A](value: A): Syntax[A] =
    Syntax.Success(value)

  /** Syntax that describes a single token of a given `kind`.
    *
    * @param kind The kind of the accepted tokens.
    *
    * @group combinator
    */
  implicit def elem[A](kind: Kind[A]): Syntax[A] =
    Syntax.Elem(kind)

  /** Recursive syntax.
    *
    * @group combinator
    */
  def recursive[A](syntax: => Syntax[A]): Syntax[A] =
    Syntax.Recursive.create(syntax)

  /** Repetition of `syntax` 0 or more times.
    *
    * @group combinator
    */
  def many[A](syntax: Syntax[A]): Syntax[Vector[A]] =
    syntax.repeatLeft(Vector[A]())(_ :+ _)

  /** Repetition of `syntax` 1 or more times.
    *
    * @group combinator
    */
  def many1[A](syntax: Syntax[A]): Syntax[Vector[A]] = {
    (syntax ~ many(syntax)).map {
      case x ~ xs => x +: xs
    }
  }

  /** Repetition of `rep` 0 or more times, separated by `sep`.
    *
    * @group combinator
    */
  def repsep[A, B](rep: Syntax[A], sep: Syntax[B]): Syntax[(Vector[A], Vector[B])] =
    rep1sep(rep, sep).opt((Vector[A](), Vector[B]()))

  /** Repetition of `rep` 1 or more times, separated by `sep`.
    *
    * @group combinator
    */
  def rep1sep[A, B](rep: Syntax[A], sep: Syntax[B]): Syntax[(Vector[A], Vector[B])] = {
    val shared = rep.shared

    lazy val res: Syntax[(Vector[A], Vector[B])] = recursive {
      (res ~ sep ~ shared).map {
        case (rs, ss) ~ s ~ r => (rs :+ r, ss :+ s)
      } | shared.map(r => (Vector[A](r), Vector[B]()))
    }

    res
  }

  /** Repetition of an `operand` 1 or more times, separated by an infix binary `operator`.
    *
    * Values are aggregated from left to right.
    *
    * @param operator The syntax for the binary operator.
    *
    * @group combinator
    */
  def infixLeft[A](operand: Syntax[A])(operator: Syntax[(A, A) => A]): Syntax[A] = {
    val shared = operand.shared

    lazy val res: Syntax[A] = recursive {
      (res ~ operator ~ shared).map {
        case x ~ f ~ y => f(x, y)
      } | shared
    }

    res
  }

  /** Repetition of an `operand` 1 or more times, separated by an infix binary `operator`.
    *
    * Values are aggregated from right to left.
    *
    * @param operator The syntax for the binary operator.
    *
    * @group combinator
    */
  def infixRight[A](operand: Syntax[A])(operator: Syntax[(A, A) => A]): Syntax[A] = {
    lazy val res: Syntax[A] = recursive {
      (operand ~ (operator ~ res).opt).map {
        case x ~ Some(f ~ y) => f(x, y)
        case x ~ None => x
      }
    }

    res
  }
}