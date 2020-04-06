package eclair
package traits

import java.nio.file._
import java.util.IdentityHashMap
import scala.collection.mutable.{ Queue, Map, HashMap, StringBuilder }
import sys.process._

/** Contains utility methods to visualise parsers.
  *
  * @group trait
  */
trait Visualisers { self: Graphs with Parsers with Results =>

  /** Outputs a diagram representation of the parser in GraphViz DOT format.
    *
    * @group visualisation
    */
  def visualise(parser: Parser[_]): String = {
    val vis = new Visualiser
    vis.add(parser)
    vis.build()
  }

  /** Creates a diagram of the parser using GraphViz DOT.
    * This method assumes that `dot` from GraphViz is installed and in `PATH`.
    *
    * @param parser    The parser to visualise.
    * @param directory The directory under which to save the `.dot` and `.pdf` files.
    * @param name      The name to use for the `.dot` and `.pdf` files.
    *
    * @group visualisation
    */
  def visualise(parser: Parser[_], directory: String, name: String): Unit = {
      val content = visualise(parser)

      val dotPath = Paths.get(directory, name + ".dot")
      val pdfPath = Paths.get(directory, name + ".pdf")

      Files.write(dotPath, content.getBytes())

      ("dot " + dotPath + " -Tpdf -o" + pdfPath).!
  }

  private class Visualiser {

    private var funIds: Map[Any, Int] = Map.empty

    private val todo: Queue[() => Unit] = new Queue

    private val recIds: Map[Int, Int] = new HashMap
    private val joinIds: Map[Int, Int] = new HashMap

    private val nodes: StringBuilder = new StringBuilder
    private val edges: StringBuilder = new StringBuilder

    private val values: IdentityHashMap[Result[Any], Int] = new IdentityHashMap

    private def getFunId(function: Any): Int = funIds.get(function) match {
      case Some(id) => id
      case None => {
        val id = funIds.size
        funIds += function -> id
        id
      }
    }

    private var nextId = 0
    private def freshId(): Int = {
      val res = nextId
      nextId += 1
      res
    }
    private def addSyntaxNode(text: String): Int = {
      val id = freshId()
      val escaped = text.replace("\"", "\\\"")
      nodes ++= s"""$id[shape=ellipse, label="$escaped"];\n"""

      id
    }
    private def addContextNode(text: String): Int = {
      val id = freshId()
      val escaped = text.replace("\"", "\\\"")
      nodes ++= s"""$id[shape=octagon, label="$escaped"];\n"""

      id
    }
    private def addResultNode(res: Result[Any]): Int = {
      if (!values.containsKey(res)) {
        val value = resultBuilder.get(res)
        val id = freshId()
        val escaped = value.toString.replace("\"", "\\\"")
        nodes ++= s"""$id[shape=box, label="$escaped"];\n"""
        values.put(res, id)
        id
      }
      else {
        values.get(res)
      }
    }
    private def addDownward(up: Int, down: Int, port: String=""): Unit = {
      edges ++= s"$up$port -> $down;\n"
    }
    private def addUpward(up: Int, down: Int, weak: Boolean, port: String=""): Unit = {
      if (weak) {
        edges ++= s"$up$port -> $down [dir=back, constraint=false];\n"
      }
      else {
        edges ++= s"$up$port -> $down [dir=back];\n"
      }
    }
    private def addFocus(id: Int): Unit = {
      edges ++= s"root -> $id [style=dotted, constraint=false];\n"
    }

    private def handle(syntax: Syntax[_]): Int = syntax match {
      case Syntax.Success(value) => addSyntaxNode("ε")
      case Syntax.Failure => addSyntaxNode("⊥")
      case Syntax.Elem(kind) => addSyntaxNode(kind.name)
      case Syntax.Disjunction(left, right) => {
        val id = addSyntaxNode("∨")
        todo += { () =>
          val lid = handle(left)
          val rid = handle(right)
          addDownward(id, lid, ":sw")
          addDownward(id, rid, ":se")
        }
        id
      }
      case Syntax.Sequence(left, right) => {
        val id = addSyntaxNode("∙")
        todo += { () =>
          val lid = handle(left)
          val rid = handle(right)
          addDownward(id, lid, ":sw")
          addDownward(id, rid, ":se")
        }
        id
      }
      case Syntax.Transform(inner, function) => {
        val id = addSyntaxNode(s"f${getFunId(function)} ⊙")
        todo += { () =>
          val iid = handle(inner)
          addDownward(id, iid, ":s")
        }
        id
      }
      case Syntax.Recursive(inner, recId) => recIds.get(recId) match {
        case Some(id) => id
        case None => {
          val id = addSyntaxNode("rec")
          recIds += recId -> id
          todo += { () =>
            val iid = handle(inner)
            addDownward(id, iid, ":s")
          }
          id
        }
      }
    }
    private def handle(focused: FocusedSyntax[_]): Int = focused match {
      case FocusedSyntax.Pair(focus, context) => {
        val fid = handle(focus)
        handle(context, fid)
        fid
      }
    }
    private def handle(focused: FocusedResult[_]): Int = focused match {
      case FocusedResult.Pair(focus, context) => {
        val fid = addResultNode(focus)
        handle(context, fid)
        fid
      }
    }
    private def handle(context: Context[_, _], parentId: Int, weak: Boolean = false): Unit = context match {
      case Context.Empty() => ()
      case Context.Prepend(result, tail) => {
        val id = addContextNode("∙")
        val rid = addResultNode(result)
        addDownward(id, rid, ":sw")
        addUpward(id, parentId, weak, ":se")
        todo += { () =>
          handle(tail, id)
        }
      }
      case Context.FollowBy(syntax, tail) => {
        val id = addContextNode("∙")
        addUpward(id, parentId, weak, ":sw")
        todo += { () =>
          handle(tail, id)
        }
        todo += { () =>
          val sid = handle(syntax)
          addDownward(id, sid, ":se")
        }
      }
      case Context.Apply(function, tail) => {
        val id = addContextNode(s"f${getFunId(function)} ⊙")
        addUpward(id, parentId, weak)
        todo += { () =>
          val tid = handle(tail, id)
        }
      }
      case Context.Join(joinId, tail) => joinIds.get(joinId) match {
        case Some(id) => addUpward(id, parentId, weak)
        case None => {
          val id = addContextNode("∨")
          addUpward(id, parentId, weak)
          joinIds += joinId -> id
          todo += { () =>
            handle(tail, id)
          }
        }
      }
      case Context.Share(tail, rest) => {
        val id = addContextNode("rec")
        addUpward(id, parentId, weak)
        todo += { () =>
          handle(tail, id)
        }
        for (other <- rest) {
          todo += { () =>
            handle(other, id, weak=true)
          }
        }
      }
    }

    def add(parser: Parser[_]): Unit = parser match {
      case s: Syntax[_] => {
        val id = handle(s)
        addFocus(id)
      }
      case Alternatives(fs, rs) => {
        for (f <- fs) {
          val fid = handle(f)
          addFocus(fid)
        }
        for (r <- rs) {
          val rid = addResultNode(r)
          addFocus(rid)
        }
      }
      case _ => throw new IllegalArgumentException("Unsupported parser.")
    }

    def build(): String = {

      while(todo.nonEmpty) {
        val action = todo.dequeue()
        action()
      }

      s"""digraph {\nordering=out;\n{ rank=source; root[label="Focal points", shape=plaintext]; }\n$nodes\n$edges}"""
    }
  }
}