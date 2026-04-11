package cps.monads

/*
TODO:  enable when js.await will be available on scala-3
  js.await is now available on 3.8+, but it must be lexically inside a js.async { ... } block,
  so CpsRuntimeAwait can't use it directly — needs deeper integration with the CPS transformation.
import cps.*

import scalajs.*
import scala.concurrent.Future
import scala.util.*

given FutureRuntimeAwait: CpsRuntimeAwait[Future] with {

  def await[A](fa: Future[A])(ctx: CpsTryMonadContext[Future]): A = {
    import scalajs.concurrent.JSExecutionContext.Implicits.given
    val jsPromise = new js.Promise[A]((resolve, reject) => {
      fa.onComplete {
        case Success(r)  => resolve(r)
        case Failure(ex) => reject(ex)
      }
    })
    js.await(jsPromise)
  }

}
 */
