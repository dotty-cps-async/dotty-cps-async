package cps.preprocessor

import cps.*

/**
 * Example tracing preprocessor that logs every val definition inside async blocks.
 * This is a reference implementation for the CpsPreprocessor documentation.
 *
 * Usage:
 * {{{
 * import cps.preprocessor.TracingPreprocessor.given
 *
 * async[ComputationBound] {
 *   val x = 1 + 1
 *   val y = x * 2
 *   y + 10
 * }
 * // Logs:
 * //   x = 2
 * //   y = 4
 * }}}
 */
object TracingPreprocessor:

  // Thread-safe log storage for testing
  private val logBuffer = new java.util.concurrent.ConcurrentLinkedQueue[String]()

  def log[T](name: String, value: T): Unit =
    val entry = s"$name = $value"
    logBuffer.add(entry)

  def getLog: List[String] =
    import scala.jdk.CollectionConverters.*
    logBuffer.asScala.toList

  def clearLog(): Unit =
    logBuffer.clear()

  // The preprocessor given - delegates to macro in separate file
  given [C <: CpsMonadContext[ComputationBound]]: CpsPreprocessor[ComputationBound, C] with
    transparent inline def preprocess[A](inline body: A, inline ctx: C): A =
      ${ TracingPreprocessorMacro.impl[A, C]('body, 'ctx) }
