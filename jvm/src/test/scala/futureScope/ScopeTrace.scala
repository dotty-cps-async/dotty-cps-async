package futureScope

import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import futureScope.util.TimeOperations

/** Events of a scope and of its children (spawn, cancel, completion, escalation, timers), to see what happened, when a test of a
  * scope fails or does not complete. Given to the root scope (`FutureScope(ec, Some(trace))`), inherited by child contexts.
  */
class ScopeTrace {

  private val start = System.nanoTime()
  private val events = new ConcurrentLinkedQueue[String]()
  private val contexts = new ConcurrentLinkedQueue[FutureScopeContext]()

  def register(ctx: FutureScopeContext): Unit =
    contexts.add(ctx)

  def apply(ctx: FutureScopeContext, event: String): Unit =
    val ms = (System.nanoTime() - start) / 1000000
    events.add(f"+$ms%6d ms [${Thread.currentThread().getName}] scope#${ctx.id}: $event")

  /** The events, the states of the scopes and the stacks of the threads which run them: the global execution context and the timer
    * of TimeOperations.
    */
  def report(): String = {
    val sb = new StringBuilder()
    sb.append("events:\n")
    events.asScala.foreach(e => sb.append("  ").append(e).append('\n'))
    sb.append("scopes:\n")
    contexts.asScala.foreach(c => sb.append("  ").append(c.debugState).append('\n'))
    sb.append("threads:\n")
    Thread.getAllStackTraces.asScala.toSeq
      .filter((t, _) =>
        t.getName.startsWith("scala-execution-context-global") || t.getName.startsWith(TimeOperations.timerThreadName)
      )
      .sortBy((t, _) => t.getName)
      .foreach { (t, stack) =>
        sb.append(s"  ${t.getName} ${t.getState}\n")
        stack.take(12).foreach(frame => sb.append(s"      at $frame\n"))
      }
    sb.toString
  }

}

object ScopeTrace {

  /** Waits for `f`, as `FutureCompleter` (a JUnit test method returns `Unit`). When `f` fails or does not complete in `timeout`,
    * prints the report of `trace` and rethrows.
    */
  def await[T](f: Future[T], trace: ScopeTrace, timeout: FiniteDuration = 30.seconds): Unit =
    try Await.result(f, timeout)
    catch
      case NonFatal(ex) =>
        System.err.println(s"=== scope trace, $ex ===\n${trace.report()}=== end of scope trace ===")
        throw ex

}
