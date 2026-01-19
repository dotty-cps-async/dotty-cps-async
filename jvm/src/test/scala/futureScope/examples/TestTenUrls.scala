package futureScope.examples

import java.io.IOException
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.*
import cps.*
import cps.monads.{*, given}
import futureScope.*
import cps.*
import cps.monads.{*, given}
import cps.util.FutureCompleter
import cps.testconfig.given
import org.junit.{Ignore, Test}
import org.junit.Assert.*


class TestTenUrls {

    enum FetchResult {
      case Success(data: String, delay: FiniteDuration)
      case Failure(msg: String)
      case InfiniteWait
    }

    import scala.concurrent.ExecutionContext.Implicits.global

    class NetworkApiMock(records:Map[String,FetchResult]) extends TenUrls.NetworkApi {


      override def fetch(url: String)(using ctx: FutureScopeContext): Future[String] = async[Future].in(Scope.child(ctx)) {
        records.get(url) match
          case Some(record) =>
            record match
              case FetchResult.Success(data, delay) =>
               await(FutureScope.spawnDelay(delay))
               data
              case FetchResult.Failure(msg) =>
               throw new IOException(msg)
              case FetchResult.InfiniteWait =>
               val p = Promise[String]
               await(p.future)
          case None =>
              throw new IOException(s"Mock URL not found $url")
      }


    }


    @Test def testSimplePages() = {
      val urlsData = (for(i <- 1 to 100) yield {
          (i.toString, FetchResult.Success(i.toString,i.milliseconds))
      } ).toMap
      val mockApi = NetworkApiMock(urlsData)
      val urls = urlsData.keys.toList
      val f = async[Future].in(Scope) {
        val first10 = await(TenUrls.readFirstN(mockApi,urls,10))
        assert(first10.length == 10)
      }
      //Await.ready(f)
      FutureCompleter(f)
    }

    @Test def testRandomBehavious() = {
      FutureScopeContext.debugCancellation = true
      val random = new Random(1)
      val urlsData = (for(i <- 1 to 100) yield {
           val p = random.nextDouble()
           val fetchResult = {
             if (p < 0.555) then
                FetchResult.Success(i.toString, random.nextInt(100).milliseconds)
             else if (p < 0.8888) then
                FetchResult.Failure(s"p=$p")
             else
                FetchResult.InfiniteWait
           }
           (i.toString, fetchResult)
      } ).toMap
      val mockApi = NetworkApiMock(urlsData)
      val urls = urlsData.keys.toList
      val f = async[Future].in(Scope) {
          FutureScope.spawnTimeout(5.seconds)
          val first10 = await(TenUrls.readFirstN(mockApi,urls,10))
          assert(first10.length == 10)
      }
      // With ~57 success URLs (0-100ms each), getting 10 should complete in <100ms.
      // Timeout (5s) should never trigger. If it does, it's a bug worth investigating.
      val nSuccess = urlsData.count(_._2.isInstanceOf[FetchResult.Success])
      val nFailure = urlsData.count(_._2.isInstanceOf[FetchResult.Failure])
      val nInfinite = urlsData.count(_._2 == FetchResult.InfiniteWait)
      FutureCompleter(f.transform{
        case Success(x) => Success(x)
        case Failure(ex) =>
           // Diagnostic info for debugging unexpected failures (race condition?)
           System.err.println("=== TestTenUrls.testRandomBehavious failed ===")
           System.err.println(s"Exception type: ${ex.getClass.getName}")
           System.err.println(s"Exception message: ${ex.getMessage}")
           System.err.println(s"Exception cause: ${Option(ex.getCause).map(c => s"${c.getClass.getName}: ${c.getMessage}").getOrElse("null")}")
           System.err.println(s"urlsData distribution: success=$nSuccess, failure=$nFailure, infinite=$nInfinite")
           System.err.println("=== end diagnostic info ===")
           Failure(ex)
      }.andThen { _ => FutureScopeContext.debugCancellation = false })
    }



}
