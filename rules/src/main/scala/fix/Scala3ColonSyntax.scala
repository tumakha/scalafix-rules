package fix

import scalafix.v1._
import scala.meta._

/**
 * Scala 3 colon syntax rewrite rule.
 *
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

  /**
   * Detect forbidden constructs inside template
   */
  private def containsForbiddenStats(templ: Template): Boolean = {
    templ.stats.exists { stat =>
      stat.exists {
        // 🚫 string literals & interpolations
        case _: Lit.String       => true
        case _: Term.Interpolate => true

        // 🚫 method implementations
        case _: Defn.Def => true

        // 🚫 imports
        case _: Import => true

        // 🚫 match expressions
        case _: Term.Match => true

        // 🚫 map / flatMap (common FP chains)
        case Term.Apply(Term.Select(_, name), _)
          if name.value == "map" || name.value == "flatMap" =>
          true

        // 🚫 async / block-style calls (e.g. Action.async { ... })
        case Term.Apply(_, args) =>
          args.exists {
            case _: Term.Block => true
            case _             => false
          }

        case _ => false
      }
    }
  }

  private def rewriteTemplate(templ: Template)(implicit doc: SemanticDocument): Patch = {

    if (templ.stats.isEmpty) return Patch.empty

    // 🚫 Skip unsafe templates
    if (containsForbiddenStats(templ)) return Patch.empty

    val tokens = templ.tokens

    // Replace first `{` with `:`
    val openBracePatch =
      tokens.collectFirst { case t: Token.LeftBrace => t }
        .map(t => Patch.replaceToken(t, ":"))
        .getOrElse(Patch.empty)

    // Remove last `}` + optional newline
    val closeBracePatch =
      tokens.reverse.collectFirst { case t: Token.RightBrace => t } match {
        case Some(rbrace) =>
          val idx = tokens.indexOf(rbrace)

          val nextToken = tokens.lift(idx + 1)

          val removeClose = Patch.removeToken(rbrace)

          val removeNewline =
            nextToken.collect {
              case t: Token.LF => Patch.removeToken(t)
            }.getOrElse(Patch.empty)

          removeClose + removeNewline

        case None =>
          Patch.empty
      }

    openBracePatch + closeBracePatch
  }
}