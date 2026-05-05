package fix

import scalafix.v1._

import scala.meta._

/**
 * @author Yuriy Tumakha
 */
class BracesToIndentation extends SemanticRule("BracesToIndentation") {

  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      case cls: Defn.Class  => rewrite(cls)
      case trt: Defn.Trait  => rewrite(trt)
      case obj: Defn.Object => rewrite(obj)
    }.asPatch

  private def rewrite(defn: Tree): Patch = {
    val templ = defn match {
      case c: Defn.Class  => c.templ
      case t: Defn.Trait  => t.templ
      case o: Defn.Object => o.templ
    }

    val tokens = templ.tokens

    val openBrace  = tokens.find(_.is[Token.LeftBrace])
    val closeBrace = tokens.find(_.is[Token.RightBrace])

    val patches = List.newBuilder[Patch]

    // 1. Replace `{` with `:`
    openBrace.foreach { t =>
      patches += Patch.replaceToken(t, ":")
    }

    // 2. Remove `}` completely (only structural brace)
    closeBrace.foreach { t =>
      patches += Patch.removeToken(t)
    }

    patches.result().asPatch
  }

}
