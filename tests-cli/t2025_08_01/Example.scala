//--disabled> using jvm "temurin:21"

//--> using scala 3.7.1

//> using dep io.github.dotty-cps-async::dotty-cps-async:1.1.2



import cps.*



trait LCHCursor {


}

object LCHCursor {

  def fromLCJson(json:LCJson): LCHCursor =
    ???

}

sealed class LCJson {

  def hcursor: LCHCursor = LCHCursor.fromLCJson(this)

}

trait LCAsync[F[_]] {
  def async_[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
}


class LCAsyncCpsMonad[F[_]:LCAsync] extends CpsTryMonad[F] with CpsTryMonadInstanceContext[F] {
}

  


given [F[_]: LCAsync] : CpsTryMonad[F] = LCAsyncCpsMonad[F]()

object Example:

  
  def convertEitherStringToF[F[_]: LCAsync] = new CpsMonadConversion[[A] =>> Either[String, A], F]:
    def apply[A](e: Either[String, A]) = 
         ???
         //e.leftMap(err => new RuntimeException(err)).liftTo[F]
  given eitherStringToFAsyncGiven[F[_]: LCAsync]: CpsMonadConversion[[A] =>> Either[String, A], F] = convertEitherStringToF


  def lookupBreak[F[_]: LCAsync](e: Either[String, LCJson]): F[LCHCursor] = async[F]:
        e.await.hcursor
        //e.get.hcursor



