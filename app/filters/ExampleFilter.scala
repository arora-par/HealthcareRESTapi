package filters

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import javax.inject._

import akka.stream.Materializer
import play.api.libs.json.{JsDefined, JsObject, JsString, Json}
import play.api.mvc.Results._
import play.api.mvc._
import play.filters.cors.CORSFilter
import services.Encryption

import scala.concurrent.{ExecutionContext, Future}
/**
 * This is a simple filter that adds a header to all requests. It's
 * added to the application's list of filters by the
 * [[Filters]] class.
 *
 * @param mat This object is needed to handle streaming of requests
 * and responses.
 * @param exec This class is needed to execute code asynchronously.
 * It is used below by the `map` method.
 */
@Singleton
class ExampleFilter @Inject()(corsFilter: CORSFilter)(
    implicit override val mat: Materializer,
    exec: ExecutionContext) extends Filter {

  override def apply(nextFilter: RequestHeader => Future[Result])
           (requestHeader: RequestHeader): Future[Result] = {
    // Run the next filter in the chain. This will call other filters
    // and eventually call the action. Take the result and modify it
    // by adding a new header.

    val key = "My very own, very private key here!"

    if (requestHeader.method.toUpperCase == "OPTIONS") {
      nextFilter(requestHeader).map { result =>
        result
      }
    } else {
      requestHeader.headers.toSimpleMap.get("Authorization") match {
        case Some(g) =>
          val token = g.stripPrefix("Bearer ")
          try
          {
            val decryptedVal = Encryption.decrypt(key, token)
            Json.parse(decryptedVal) match {
              case q:JsObject =>
                println(s"userName: ${q \ "userName"}")
                println(s"role: ${q \ "role"}")
                println(s"organization: ${q \ "organization"}")
                println(s"ttl: ${q \ "ttl"}")
                // CONSIDER setting up role based access rules
                // validate 'time to live' on the token
                q \ "ttl" match {
                  case JsDefined(x) =>
                    val ttlStr = x.as[JsString] value
                    val isoInstant = LocalDate.parse(ttlStr, DateTimeFormatter.ISO_LOCAL_DATE_TIME);
                    if(isoInstant.isAfter(LocalDate.now())) {
                      println(s"valid ttl on token: $isoInstant")
                      nextFilter(requestHeader).map { result =>
                        result
                      }
                    } else {
                      Future(Unauthorized.withHeaders("WWW-Authenticate" -> """Bearer realm="healthcare",error="invalid_token",error_description="expired ttl""""))
                    }
                  case _ => Future(Unauthorized.withHeaders("WWW-Authenticate" -> """Bearer realm="healthcare",error="invalid_token",error_description="missing ttl""""))
                }
              case _ => Future(Unauthorized.withHeaders("WWW-Authenticate" -> """Bearer realm="healthcare",error="invalid_token""""))
            }
          }
          catch
            {
              case e: Throwable => println(s"Got exception while decrypting: ${e.getMessage}")
                Future(Unauthorized.withHeaders("WWW-Authenticate" -> """Bearer realm="healthcare",error="invalid_token""""))
            }
        case None =>
          Future(BadRequest.withHeaders("WWW-Authenticate" -> """Bearer realm="healthcare",error="invalid_request",error_description="missing Authorization header""""))
      }
    }
  }
}
