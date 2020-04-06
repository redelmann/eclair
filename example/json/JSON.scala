
package json

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import eclair._

object JSON {
  sealed trait Token
  case class StringToken(value: String) extends Token
  case class BooleanToken(value: Boolean) extends Token
  case class NumberToken(value: Double) extends Token
  case class NullToken() extends Token
  case class SquareBracketToken(isOpen: Boolean) extends Token
  case class CurlyBracketToken(isOpen: Boolean) extends Token
  case class ColonToken() extends Token
  case class CommaToken() extends Token
  case class UnknownToken(content: String) extends Token

  sealed trait Value
  case class StringValue(value: String) extends Value
  case class BooleanValue(value: Boolean) extends Value
  case class NumberValue(value: Double) extends Value
  case class NullValue() extends Value
  case class ArrayValue(values: Seq[Value]) extends Value
  case class ObjectValue(values: Seq[Binding]) extends Value

  case class Binding(key: StringValue, value: Value)
}
import JSON._

class JSONLexer(source: Iterator[Char]) {

  var in = source
  val out = new ArrayBuffer[Token]()

  var start = 0
  var end = 0

  @tailrec
  private def next(): Char = {
    var char = in.next(); end += 1
    if (char.isWhitespace) {
      start += 1
      next()
    }
    else {
      char
    }
  }

  @inline
  private def range = (start, end)

  @inline
  private def isHex(char: Char): Boolean =
    char.isDigit || (char >= 'A' && char <= 'F') || (char >= 'a' && char <= 'f')

  private def goNumber(char: Char): Unit = {
    val content = new StringBuilder()
    var c = char

    try {
      if (c == '-') {
        content += c; c = in.next(); end += 1
      }

      if (c == '0') {
        content += c; c = in.next(); end += 1
      }
      else {

        if (c < '0' || c > '9') { out.+=(UnknownToken(content.toString)); return }

        while (c >= '0' && c <= '9') {
          content += c; c = in.next(); end += 1
        }
      }

      if (c == '.') {
        content += c; c = in.next(); end += 1

        if (c < '0' || c > '9') { out.+=(UnknownToken(content.toString)); return }

        while (c >= '0' && c <= '9') {
          content += c; c = in.next(); end += 1
        }
      }

      if (c == 'E' || c == 'e') {
        content += c; c = in.next(); end += 1

        if (c == '+' || c == '-') {
          content += c; c = in.next(); end += 1
        }

        if (c < '0' || c > '9') { out.+=(UnknownToken(content.toString)); return }

        while (c >= '0' && c <= '9') {
          content += c; c = in.next(); end += 1
        }
      }

      end -= 1
      out.+=(NumberToken(content.toString.toDouble))
      end += 1
    }
    catch {
      case e: java.util.NoSuchElementException => {
        try {
          out.+=(NumberToken(content.toString.toDouble))
          start = end
        }
        catch {
          case _: Exception => ()
        }
        throw e
      }
    }

    start = end - 1

    while (c.isWhitespace) {
      c = in.next(); start = end; end += 1;
    }

    go(c)
  }

  private def go(char: Char): Unit = {
    var c = char
    char match {
      case '[' => out.+=(SquareBracketToken(true))
      case ']' => out.+=(SquareBracketToken(false))
      case '{' => out.+=(CurlyBracketToken(true))
      case '}' => out.+=(CurlyBracketToken(false))
      case ',' => out.+=(CommaToken())
      case ':' => out.+=(ColonToken())
      case 't' => {
        c = in.next(); end += 1
        if (c != 'r') { out.+=(UnknownToken("t" + c)); return }
        c = in.next(); end += 1
        if (c != 'u') { out.+=(UnknownToken("tr" + c)); return }
        c = in.next(); end += 1
        if (c != 'e') { out.+=(UnknownToken("tru" + c)); return }
        out.+=(BooleanToken(true))
      }
      case 'f' => {
        c = in.next(); end += 1
        if (c != 'a') { out.+=(UnknownToken("f" + c)); return }
        c = in.next(); end += 1
        if (c != 'l') { out.+=(UnknownToken("fa" + c)); return }
        c = in.next(); end += 1
        if (c != 's') { out.+=(UnknownToken("fal" + c)); return }
        c = in.next(); end += 1
        if (c != 'e') { out.+=(UnknownToken("fals" + c)); return }
        out.+=(BooleanToken(false))
      }
      case 'n' => {
        c = in.next(); end += 1
        if (c != 'u') { out.+=(UnknownToken("n" + c)); return }
        c = in.next(); end += 1
        if (c != 'l') { out.+=(UnknownToken("nu" + c)); return }
        c = in.next(); end += 1
        if (c != 'l') { out.+=(UnknownToken("nul" + c)); return }
        out.+=(NullToken())
      }
      case '"' => {
        val content = new StringBuilder()
        while (true) {
          c = in.next(); end += 1
          if (c == '"') { out.+=(StringToken(content.toString)); return }

          content += c

          if (c == '\\') {
            c = in.next(); end += 1; content += c
            c match {
              case '\\' | '"' | '/' | 'b' | 'f' | 'n' | 'r' | 't' => ()
              case 'u' => {
                c = in.next(); end += 1; content += c
                if (!isHex(c)) { out.+=(UnknownToken("\"" + content.toString)); return }
                c = in.next(); end += 1; content += c
                if (!isHex(c)) { out.+=(UnknownToken("\"" + content.toString)); return }
                c = in.next(); end += 1; content += c
                if (!isHex(c)) { out.+=(UnknownToken("\"" + content.toString)); return }
                c = in.next(); end += 1; content += c
                if (!isHex(c)) { out.+=(UnknownToken("\"" + content.toString)); return }
              }
              case _ => out.+=(UnknownToken("\"" + content.toString))
            }
          }
          else if (c.isControl) {
            out.+=(UnknownToken("\"" + content.toString)); return
          }
        }
      }
      case _ => goNumber(c)
    }
  }

  def toIterator: Iterator[Token] = {

    try {
      while(true) {
        start = end
        go(next())
      }
    }
    catch {
      case e: java.util.NoSuchElementException => {
        if (start != end) {
          out.+=(UnknownToken(""))
        }
      }
    }

    out.iterator
  }
}

object JSONLexer {

  def apply(it: Iterator[Char]): Iterator[Token] =
    new JSONLexer(it).toIterator
}

object JSONParser {


  val eclairLibrary = library[Token]
  import eclairLibrary._

  val stringValue: Kind[StringValue] = acceptWith("string") {
    case StringToken(value) => StringValue(value)
  }

  val booleanValue: Kind[BooleanValue] = acceptWith("boolean") {
    case BooleanToken(value) => BooleanValue(value)
  }

  val numberValue: Kind[NumberValue] = acceptWith("number") {
    case NumberToken(value) => NumberValue(value)
  }

  val nullValue: Kind[NullValue] = acceptWith("null") {
    case NullToken() => NullValue()
  }

  val openSquare: Kind[Token] = acceptWhen("[")(_ == SquareBracketToken(true))
  val closeSquare: Kind[Token] = acceptWhen("]")(_ == SquareBracketToken(false))
  val openCurly: Kind[Token] = acceptWhen("{")(_ == CurlyBracketToken(true))
  val closeCurly: Kind[Token] = acceptWhen("}")(_ == CurlyBracketToken(false))
  val colon: Kind[Token] = acceptWhen(":")(_ == ColonToken())
  val comma: Kind[Token] = acceptWhen(",")(_ == CommaToken())

  lazy val arrayValue: Syntax[ArrayValue] =
    (openSquare.hide ~ repsep(value, comma) ~ closeSquare.hide).map {
      case (values, _) => ArrayValue(values)
    }

  lazy val binding: Syntax[Binding] = {
    (stringValue ~ colon.hide ~ value).map {
      case key ~ value => Binding(key, value)
    }
  }

  lazy val objectValue: Syntax[ObjectValue] = {
    (openCurly.hide ~ repsep(binding, comma) ~ closeCurly.hide).map {
      case (bindings, _) => ObjectValue(bindings)
    }
  }

  lazy val value: Syntax[Value] = recursive {
    stringValue | booleanValue | numberValue | nullValue | arrayValue | objectValue
  }

  def apply(tokens: Iterator[Token]): Option[Value] = {
    value(tokens).get
  }

  def main(args: Array[String]): Unit = {
    val input = """[1, {"foo": 2.3, "bar": true}, [[{}, []]], null]"""
    val tokens = JSONLexer(input.iterator)
    var parser: Parser[Value] = value
    visualise(parser, "/tmp/", "original")
    var i = 0
    for (token <- tokens) {
      i += 1
      println(i)
      parser = parser(Iterator(token)).rest
      visualise(parser, "/tmp/", "after-token-" + i)
    }
  }
}

object Runner {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  def apply(file: String): Option[Value] = {
    time(JSONParser(JSONLexer(scala.io.Source.fromFile(file))))
  }

  def loop(file: String): Unit = {
    while (true) {
      JSONParser(JSONLexer(scala.io.Source.fromFile(file)))
    }
  }
}