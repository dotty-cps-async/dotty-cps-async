package cps.runtime.util.control

import cps.*
import scala.util.*
import scala.util.control.*

class BreaksAsyncShift extends AsyncShift[Breaks.type] {

  def break(o: Breaks.type): Nothing = {
    try {
      Breaks.break()
    } catch {
      case ex: ControlThrowable =>
        throw new ControlThrowableAsyncWrapper(ex)
    }
  }

  /** Sync counterpart of `breakable` used by the macro when the body contains no
    * await but a `NonFatal` substitution was applied. Catches a wrapped break and
    * delegates to stdlib `Breaks.breakable` to honour identity matching.
    */
  def syncBreakable(o: Breaks.type)(op: () => Unit): Unit = {
    try op()
    catch
      case w: ControlThrowableAsyncWrapper =>
        Breaks.breakable { throw w.ce }
  }

  def breakable[F[_]](o: Breaks.type, m: CpsTryMonad[F])(op: () => F[Unit]): F[Unit] = {
    val opF: F[Unit] =
      try op()
      catch
        case NonFatal(ex) => m.error(ex)
        case ex: ControlThrowable =>
          Breaks.breakable { throw ex }
          m.pure(())
    m.flatMapTry(opF) {
      case Success(v) => m.pure(v)
      case Failure(ex) =>
        val toThrow = ex match
          case w: ControlThrowableAsyncWrapper => w.ce
          case other                           => other
        try {
          Breaks.breakable { throw toThrow }
          m.pure(())
        } catch {
          case NonFatal(ex2) => m.error(ex2)
        }
    }
  }

}

object BreaksAsyncShift extends BreaksAsyncShift
