package fix

import scalafix.v1._
import scala.meta._

/**
 * Ensures every source file ends with exactly one empty line.
 *
 * Meaning:
 * code...
 * <newline>
 *
 * Fixes:
 * - no trailing newline
 * - multiple blank lines at EOF
 * - spaces/tabs on trailing blank lines
 *
 * @author Yuriy Tumakha
 */
class SingleTrailingEmptyLine
  extends SyntacticRule("SingleTrailingEmptyLine") {

  override def fix(implicit doc: SyntacticDocument): Patch = {
    val input = doc.input.text

    // Normalize all trailing whitespace/newlines at EOF
    val trimmed = input.replaceAll("[\\s\\n\\r]+$", "")

    // Rebuild file with exactly one empty line at end
    // Final shape = content + "\n\n"
    val rewritten =
      if (trimmed.isEmpty) "\n"
      else trimmed + "\n"

    if (rewritten == input) Patch.empty
    else Patch.replaceTree(doc.tree, rewritten)
  }
}
