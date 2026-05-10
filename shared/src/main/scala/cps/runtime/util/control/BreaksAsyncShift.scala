package cps.runtime.util.control

import cps.*
import scala.util.*
import scala.util.control.*

class BreaksAsyncShift extends AsyncShift[Breaks.type] {

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
        try {
          Breaks.breakable { throw ex }
          m.pure(())
        } catch {
          case NonFatal(ex2) => m.error(ex2)
        }
    }
  }

}

object BreaksAsyncShift extends BreaksAsyncShift
