package services

import java.security.MessageDigest
import java.util.UUID

import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Results.Status
import play.api.mvc.{RequestHeader, Result}


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

  def JsonAnswer(status: Int = 200, jsObject: JsValue)(implicit request: RequestHeader): Result =
    Status(status).apply(
      Json.obj(
        "result" -> Json.obj("status" -> status),
        "data" -> jsObject
      )
    ).withToken
}
