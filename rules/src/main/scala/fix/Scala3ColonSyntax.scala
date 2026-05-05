package fix

import scalafix.v1._
import scala.meta._

/**
 * @author Yuriy Tumakha
 */
class Scala3ColonSyntax extends SemanticRule("Scala3ColonSyntax") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case cls: Defn.Class  => rewriteTemplate(cls.templ)
      case obj: Defn.Object => rewriteTemplate(obj.templ)
      case trt: Defn.Trait  => rewriteTemplate(trt.templ)
    }.asPatch
  }

  private def rewriteTemplate(templ: Template)(implicit doc: SemanticDocument): Patch = {
    // Skip templates with no stats (e.g. `class Foo`)
    if (templ.stats.isEmpty) return Patch.empty

    val tokens = templ.tokens

    val openBracePatch =
      tokens.collectFirst { case t: Token.LeftBrace => t }
        .map(t => Patch.replaceToken(t, ":"))
        .getOrElse(Patch.empty)

    val closeBracePatch =
      tokens.reverse.collectFirst { case t: Token.RightBrace => t }
        .map(Patch.removeToken)
        .getOrElse(Patch.empty)

    openBracePatch + closeBracePatch
  }
}
