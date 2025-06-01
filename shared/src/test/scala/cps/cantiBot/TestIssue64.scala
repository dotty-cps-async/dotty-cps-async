package cps.cantiBot

import cps.*
import cps.monads.FutureAsyncMonad
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class Test64 {

  given CpsSchedulingMonad[Future] = FutureAsyncMonad

  def test(): Future[Unit] = {
    // given cps.macros.flags.DebugLevel = cps.macros.flags.DebugLevel(20)
    async {
      ().thing()
    }
  }

  extension (value: Unit) def thing[X]() = ()

}
