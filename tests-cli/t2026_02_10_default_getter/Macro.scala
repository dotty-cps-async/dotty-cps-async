import scala.quoted.*

/**
 * Minimal macro that runs an identity TreeMap over the body.
 * This is enough to trigger the "-Xcheck-macros" assertion failure
 * on trees containing case class copy with default getters.
 */
object TestMacro:

  inline def identityTreeMap[T](inline body: T): T =
    ${ identityTreeMapImpl[T]('body) }

  def identityTreeMapImpl[T: Type](body: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.*
    val mapped = new TreeMap {}.transformTerm(body.asTerm)(Symbol.spliceOwner)
    mapped.asExprOf[T]
