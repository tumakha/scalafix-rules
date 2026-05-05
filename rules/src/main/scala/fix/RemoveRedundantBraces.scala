package fix

import scalafix.v1._
import scala.meta._

/**
 * @author Yuriy Tumakha
 */
class RemoveRedundantBraces extends SemanticRule("RemoveRedundantBraces") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case d: Defn.Def => rewrite(d.body)
      case v: Defn.Val => rewrite(v.rhs)
    }.asPatch
  }

  private def rewrite(body: Term)(implicit doc: SemanticDocument): Patch = body match {
    case block: Term.Block if isPureExpressionBlock(block) =>
      removeBraces(block)

    case _ =>
      Patch.empty
  }

  // ✔ Safe heuristic for Scala 3-style indentation blocks
  private def isPureExpressionBlock(block: Term.Block): Boolean = {

    val stats = block.stats

    // must not contain structural definitions
    val hasStructuralDefs = stats.exists {
      case _: Defn.Def   => true
      case _: Defn.Val   => false
      case _: Defn.Var   => true
      case _: Defn.Trait => true
      case _: Defn.Class => true
      case _: Defn.Object=> true
      case _             => false
    }

    // last statement must be an expression result
    val endsWithExpr = stats.lastOption.exists {
      case _: Term => true
      case _       => false
    }

    stats.nonEmpty && !hasStructuralDefs && endsWithExpr
  }

  private def removeBraces(block: Term.Block)(implicit doc: SemanticDocument): Patch = {
    val tokens = block.tokens

    val open = tokens.collectFirst { case t: Token.LeftBrace => t }
    val close = tokens.reverse.collectFirst { case t: Token.RightBrace => t }

    val openPatch = open match {
      case Some(o) => Patch.removeToken(o)
      case None    => Patch.empty
    }

    val closePatch = close match {
      case Some(c) => removeLine(c)
      case None    => Patch.empty
    }

    openPatch + closePatch
  }

  private def removeLine(token: Token)(implicit doc: SemanticDocument): Patch = {
    val tokens = doc.tree.tokens

    val idx = tokens.indexOf(token)

    val removeBrace = Patch.removeToken(token)

    val removePrevNewline =
      tokens.lift(idx - 1) match {
        case Some(prev: Token.LF) => Patch.removeToken(prev)
        case _                    => Patch.empty
      }

    removeBrace + removePrevNewline
  }

}
