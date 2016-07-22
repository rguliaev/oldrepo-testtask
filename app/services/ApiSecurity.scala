package services

import java.security.MessageDigest
import java.util.UUID
import play.api.mvc.{RequestHeader, Result}
import scala.concurrent.Future

trait ApiSecurity {
  val sessionId = "sessionId"
  def hash(s: String) = MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8")).map("%02x".format(_)).mkString("")

  implicit class ResultWithToken(result: Result)(implicit request: RequestHeader) {
    def withToken: Result = {
      request.session.get(sessionId) match {
        case Some(session) => result
        case None => result.withSession(sessionId -> hash(request.remoteAddress + UUID.randomUUID().toString))
      }
    }
  }

}
