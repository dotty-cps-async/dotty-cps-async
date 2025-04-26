package cps.monads

import cps.*

import scalajs.*
import scala.concurrent.Future
import scala.util.*

object FutureRuntimeAwait extends CpsRuntimeAwait[Future] {

  def await[A](fa: Future[A])(ctx: CpsTryMonadContext[Future]): A = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val jsPromise = new js.Promise[A]((resolve, reject) => {
      fa.onComplete {
        case Success(r)  => resolve(r)
        case Failure(ex) => reject(ex)
      }
    })
    js.await(jsPromise)
  }

}
