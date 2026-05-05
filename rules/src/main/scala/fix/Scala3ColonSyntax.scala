package fix

import scalafix.v1._
import scala.meta._

/**
 * @author Yuriy Tumakha
 */
class Scala3ColonSyntax extends SemanticRule("Scala3ColonSyntax") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case cls: Defn.Class if isTopLevel(cls) && hasSameLineBrace(cls.templ, cls) =>
        rewriteTemplate(cls.templ)

      case obj: Defn.Object if isTopLevel(obj) && hasSameLineBrace(obj.templ, obj) =>
        rewriteTemplate(obj.templ)

      case trt: Defn.Trait if isTopLevel(trt) && hasSameLineBrace(trt.templ, trt) =>
        rewriteTemplate(trt.templ)
    }.asPatch
  }

  private def isTopLevel(tree: Tree): Boolean =
    tree.parent.exists(_.is[Source])

  private def hasSameLineBrace(templ: Template, owner: Tree): Boolean = {
    val braceOpt = templ.tokens.collectFirst { case t: Token.LeftBrace => t }

    braceOpt.exists { brace =>
      val ownerEndLine = owner.pos.endLine
      val braceLine    = brace.pos.startLine
      ownerEndLine == braceLine
    }
  }

  private def isInsideForbiddenContext(tree: Tree): Boolean = {
    def loop(t: Tree): Boolean = {
      t.parent match {
        case Some(parent) =>
          parent match {
            case _: Lit.String       => true
            case _: Term.Interpolate => true
            case _: Term.Match       => true
            case _                   => loop(parent)
          }
        case None => false
      }
    }
    loop(tree)
  }

  private def rewriteTemplate(templ: Template)(implicit doc: SemanticDocument): Patch = {

    if (templ.stats.isEmpty) return Patch.empty

    // 🚫 Skip unsafe contexts
    if (isInsideForbiddenContext(templ)) return Patch.empty

    val tokens = templ.tokens

    val openBracePatch =
      tokens.collectFirst { case t: Token.LeftBrace => t }
        .map(t => Patch.replaceToken(t, ":"))
        .getOrElse(Patch.empty)

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
