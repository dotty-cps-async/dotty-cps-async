//--disabled> using jvm "temurin:21"

//--> using scala 3.7.1

//> using dep org.typelevel::cats-core:2.+
//> using dep org.typelevel::alleycats-core:2.+
//> using dep io.chrisdavenport::cats-effect-time:0.2.+
//> using dep commons-io:commons-io:2.+
//> using dep org.scala-lang.modules::scala-collection-contrib:0.4.+
//> using dep org.slf4j:slf4j-simple:2.+
//> using dep org.http4s::http4s-ember-client:0.23.+
//> using dep org.http4s::http4s-circe:0.23.+
//> using dep io.circe::circe-core:0.14.+
//> using dep io.circe::circe-generic:0.14.+
//> using dep io.circe::circe-parser:0.14.+
//> using dep io.github.dotty-cps-async::dotty-cps-async:1.1.2
//> using dep io.github.dotty-cps-async::cps-async-connect-cats-effect:1.1.0
//> using dep io.github.dotty-cps-async::cps-async-connect-cats-effect-loom:1.1.0


import cats.effect.*
import cats.effect.implicits.*

import java.time.*
import java.time.format.DateTimeFormatter

import cps.*
import cps.monads.catsEffect.{*, given}


import cats.effect.*
import cats.syntax.all.*
import org.http4s.client.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.Uri
import io.circe.*
import io.circe.parser.*

object Example:
  val assetCoindIdMap = Map(Asset.BTC -> "bitcoin")
  val yyMMyyyyFormat = DateTimeFormatter.ofPattern("dd-MM-yyyy").withZone(ZoneOffset.UTC)

  case class Asset(code: String, typ: AssetType)
  object Asset:
    val BTC = Asset("BTC", AssetType.crypto)
  enum AssetType:
    case currency
    case crypto
    case asx
    case nasdaq

  final case class LCParsingFailure(message: String, underlying: Throwable) extends RuntimeException(message, underlying)

  def lc_io_circe_parse(input: String): Either[LCParsingFailure, Json] = {
     ???
  }

  def convertEitherStringToF[F[_]: Async] = new CpsMonadConversion[[A] =>> Either[String, A], F]:
    def apply[A](e: Either[String, A]) = e.leftMap(err => new RuntimeException(err)).liftTo[F]
  given eitherStringToFAsyncGiven[F[_]: Async]: CpsMonadConversion[[A] =>> Either[String, A], F] = convertEitherStringToF

  def convertEitherThrowableToF[F[_]: Async, Ex <: Throwable] = new CpsMonadConversion[[A] =>> Either[Ex, A], F]:
    def apply[A](e: Either[Ex, A]) = e.liftTo[F]
  given eitherThrowableToFAsyncGiven[F[_]: Async, Ex <: Throwable]: CpsMonadConversion[[A] =>> Either[Ex, A], F] = convertEitherThrowableToF


  def lookupBreak[F[_]: Async](asset: Asset, time: Instant): F[BigDecimal] = async[F]:
    EmberClientBuilder.default[F].build.use: client =>
      lc_io_circe_parse("xxxx"
      ).await.hcursor.downField("market_data").downField("current_price").get[BigDecimal]("aud").liftTo[F]
    .await

  /*
  def lookupWork[F[_]: Async](asset: Asset, time: Instant): F[BigDecimal] = async[F]:
    EmberClientBuilder.default[F].build.use: client =>
      val x = parse(
        client.expect[String](
          Uri.unsafeFromString(s"https://api.coingecko.com/api/v3/coins/${
              assetCoindIdMap.get(asset).toRight(s"Asset not supported $asset").await}/history")
            .withQueryParams(Map("date" -> yyMMyyyyFormat.format(time), "localization" -> "false"))
        ).await
      ).await
      x.hcursor.downField("market_data").downField("current_price").get[BigDecimal]("aud").liftTo[F]
    .await
   */

