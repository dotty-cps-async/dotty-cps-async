package cps.runtime.util.control

import cps.*
import scala.util.*
import scala.util.control.*

object BreaksAsyncShift extends AsyncShift[Breaks.type] {

  def breakable[F[_]](o: Breaks.type, m: CpsTryMonad[F])(op: () => F[Unit]): F[Unit] =
    m.flatMapTry(op()) {
      case Success(v) => m.pure(v)
      case Failure(ex) =>
        try {
          Breaks.breakable { throw ex }
          m.pure(())
        } catch {
          case ex2: Throwable => m.error(ex2)
        }
    }

}
