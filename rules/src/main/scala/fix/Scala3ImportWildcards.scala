package fix

import scalafix.v1._
import scala.meta._

class Scala3ImportWildcards extends SyntacticRule("Scala3ImportWildcards") {

  override def fix(implicit doc: SyntacticDocument): Patch =
    doc.tree.collect {
      case Importer(ref, importees) =>
        importees.collect {
          case wildcard: Importee.Wildcard =>
            Patch.replaceTree(wildcard, "*")
        }.asPatch
    }.asPatch
}
