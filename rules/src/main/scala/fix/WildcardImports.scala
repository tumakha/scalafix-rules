package fix

import scalafix.v1._
import scala.meta._

/**
 * @author Yuriy Tumakha
 */
class WildcardImports extends SyntacticRule("WildcardImports") {

  override def fix(implicit doc: SyntacticDocument): Patch = {
    doc.tree.collect {
      case Import(importers) =>
        importers.flatMap { importer =>
          importer.importees.collect {
            case Importee.Wildcard() =>
              Patch.replaceTree(importer, s"${importer.ref.syntax}.*")
          }
        }.asPatch
    }.asPatch
  }
}
