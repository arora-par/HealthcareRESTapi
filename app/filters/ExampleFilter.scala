package filters

import javax.inject._

import akka.stream.Materializer
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Results._
import play.api.mvc._
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
class ExampleFilter @Inject()(
    implicit override val mat: Materializer,
    exec: ExecutionContext) extends Filter {

  override def apply(nextFilter: RequestHeader => Future[Result])
           (requestHeader: RequestHeader): Future[Result] = {
    // Run the next filter in the chain. This will call other filters
    // and eventually call the action. Take the result and modify it
    // by adding a new header.

    val key = "My very own, very private key here!"
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
              // TODO - Organization and role based access validation here
              nextFilter(requestHeader).map { result =>
                result
              }
            case _ =>
              Future(Unauthorized.withHeaders("WWW-Authenticate" -> """Bearer realm="example",error="invalid_token""""))
          }
        }
        catch
          {
            case e: Throwable => println(s"Got exception while decrypting: ${e.getMessage}")
              Future(Unauthorized.withHeaders("WWW-Authenticate" -> """Bearer realm="example",error="invalid_token""""))
          }
      case None =>
          Future(BadRequest.withHeaders("WWW-Authenticate" -> """Bearer realm="example",error="invalid_request",error_description="missing Authorization header""""))
    }
  }
}
