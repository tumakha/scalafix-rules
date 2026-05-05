package fix

import scalafix.v1._
import scala.meta._

/**
 * @author Yuriy Tumakha
 */
class ImplicitToUsing extends SyntacticRule("ImplicitToUsing") {

  override def fix(implicit doc: SyntacticDocument): Patch = {
    doc.tree.collect {
      case param @ Term.ParamClause(params)
        if isImplicitParamClause(param) =>
        replaceImplicitKeyword(param)
    }.asPatch
  }

  /**
   * We only allow transformation when:
   * - clause contains "implicit"
   * - AND it is NOT inside a lambda (Term.Function)
   */
  private def isImplicitParamClause(clause: Term.ParamClause): Boolean = {
    clause.tokens.exists(_.text == "implicit") &&
      !clause.parent.exists {
        case _: Term.Function => true // ❌ skip Play async/request blocks
        case _                => false
      }
  }

  private def replaceImplicitKeyword(clause: Term.ParamClause): Patch = {
    clause.tokens.collect {
      case tok if tok.text == "implicit" =>
        Patch.replaceToken(tok, "using")
    }.asPatch
  }
}
