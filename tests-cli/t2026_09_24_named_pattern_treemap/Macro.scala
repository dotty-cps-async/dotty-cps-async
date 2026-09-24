import scala.quoted.*

/**
 * Minimal macro that runs an identity TreeMap over the body.
 * This is enough to fail with MatchError on a named pattern (`case C(name = p)`).
 */
object TestMacro:

  inline def identityTreeMap[T](inline body: T): T =
    ${ identityTreeMapImpl[T]('body) }

  def identityTreeMapImpl[T: Type](body: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.*
    val mapped = new TreeMap {}.transformTerm(body.asTerm)(Symbol.spliceOwner)
    mapped.asExprOf[T]
