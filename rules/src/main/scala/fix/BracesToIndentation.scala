package fix

import scalafix.v1._

import scala.meta._

/**
 * @author Yuriy Tumakha
 */
class BracesToIndentation extends SyntacticRule("BracesToIndentation") {

  override def fix(implicit doc: SyntacticDocument): Patch = {
    doc.tree.collect {
      case cls: Defn.Class  => transformClass(cls)
      case trt: Defn.Trait  => transformTrait(trt)
      case obj: Defn.Object => transformObject(obj)
    }.asPatch
  }

  // ---------- CLASS ----------
  private def transformClass(cls: Defn.Class): Patch = {
    val header =
      s"${renderMods(cls.mods)}class ${cls.name.value}" +
        renderTParams(cls.tparams) +
        renderCtor(cls.ctor) +
        renderParents(cls.templ)

    val body = renderBody(cls.templ)

    Patch.replaceTree(cls, header + body)
  }

  // ---------- TRAIT ----------
  private def transformTrait(trt: Defn.Trait): Patch = {
    val header =
      s"${renderMods(trt.mods)}trait ${trt.name.value}" +
        renderTParams(trt.tparams) +
        renderParents(trt.templ)

    val body = renderBody(trt.templ)

    Patch.replaceTree(trt, header + body)
  }

  // ---------- OBJECT ----------
  private def transformObject(obj: Defn.Object): Patch = {
    val header =
      s"${renderMods(obj.mods)}object ${obj.name.value}" +
        renderParents(obj.templ)

    val body = renderBody(obj.templ)

    Patch.replaceTree(obj, header + body)
  }

  // ---------- HELPERS ----------

  private def renderMods(mods: List[Mod]): String =
    if (mods.isEmpty) "" else mods.map(_.syntax).mkString("", " ", " ")

  private def renderTParams(tparams: List[Type.Param]): String =
    if (tparams.isEmpty) ""
    else tparams.map(_.syntax).mkString("[", ", ", "]")

  private def renderCtor(ctor: Ctor.Primary): String =
    renderParamss(ctor.paramss)

  private def renderParamss(paramss: List[List[Term.Param]]): String =
    paramss
      .map(params => params.map(_.syntax).mkString("(", ", ", ")"))
      .mkString("")

  private def renderParents(templ: Template): String = {
    templ.parent match {
      case None => ""

      case Some(init) =>
        // init has: ctor + args + type
        val first = init.syntax

        init match {
          case _ =>
            s" extends $first"
        }
    }
  }

  private def renderBody(templ: Template): String = {
    val body = templ.stats.map(s => indent(s.syntax)).mkString("\n")
    s":\n$body"
  }

  private def indent(code: String): String =
    code.linesIterator.map("  " + _).mkString("\n")
}
