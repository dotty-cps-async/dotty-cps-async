package cps.celike

/*
class IntermediateValRequireTest {

  def lookupExp[F[_] : Async](asset: Asset, time: Instant): F[BigDecimal] = async[F]:
    EmberClientBuilder.default[F].build.use: client =>
      parse(
        client.expect[String](
          Uri.unsafeFromString(s"https://api.coingecko.com/api/v3/coins/${
              assetCoindIdMap.get(asset).toRight(s"Asset not supported $asset").await
            }/history")
            .withQueryParams(Map("date" -> yyMMyyyyFormat.format(time), "localization" -> "false"))
        ).await
      ).await.hcursor.downField("market_data").downField("current_price").get[BigDecimal]("aud").liftTo[F]
    .await

  @Test
  def testIntermediateValRequire(): Unit = {

  }

}
 */
