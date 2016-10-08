import javax.inject._

import play.api.http.DefaultHttpErrorHandler
import play.api._
import play.api.libs.json.JsString
import play.api.mvc._
import play.api.mvc.Results._
import play.api.routing.Router
import services.ApiSecurity

import scala.concurrent._

@Singleton
class ErrorHandler @Inject() (
                               env: Environment,
                               config: Configuration,
                               sourceMapper: OptionalSourceMapper,
                               router: Provider[Router]
                             ) extends DefaultHttpErrorHandler(env, config, sourceMapper, router) with ApiSecurity {

  override def onBadRequest(request: RequestHeader, message: String): Future[Result] = {
    Future.successful(
      JsonAnswer(400,JsString("Bad Request Error: " + message))(request)
    )
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    Future.successful(
      JsonAnswer(500,JsString("Internal Server Error: " + exception.getMessage))(request)
    )
  }

  override def onNotFound(request: RequestHeader, message: String): Future[Result] = {
    Future.successful(
      JsonAnswer(404,JsString("Not Found Error: " + message))(request)
    )
  }

  override def onForbidden(request: RequestHeader, message: String) = {
    Future.successful(
      Forbidden("You're not allowed to access this resource.")
    )
  }
}
