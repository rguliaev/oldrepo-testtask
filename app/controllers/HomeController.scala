package controllers

import javax.inject._
import actors.{SaveNumberList, GetNumberList, FilesMasterActor}
import akka.actor.{Props, ActorSystem}
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout
import play.api.{Configuration, Environment}
import play.api.libs.json.Json
import play.api.mvc._
import services.ApiSecurity
import scala.concurrent.duration._
import scala.concurrent.{Future, ExecutionContext}

@Singleton
class HomeController @Inject()(actorSystem: ActorSystem, environment: Environment, configuration: Configuration)(implicit exec: ExecutionContext) extends Controller with ApiSecurity {
  implicit lazy val timeout = Timeout(10 seconds)
  lazy val filesActor = actorSystem.actorOf(Props(new FilesMasterActor(environment,configuration)))
  type EitherGet = Either[String,List[BigDecimal]]
  type EitherSave = Either[String,String]

  def numbers(file: Int): Future[EitherGet] = {
    (filesActor ? GetNumberList(file)).mapTo[EitherGet].recover {
      case e: AskTimeoutException => Left(e.getMessage)
    }
  }

  def save(ls: List[BigDecimal]): Future[EitherSave] = {
    (filesActor ? SaveNumberList(ls)).mapTo[EitherSave].recover {
      case e: AskTimeoutException => Left(e.getMessage)
    }
  }

  def index = Action.async { implicit request =>
    Future.successful(Ok(views.html.index()).withToken)
  }

  def get(v1: Int) = Action.async { implicit request =>
    numbers(2).map {
      case Right(ls: List[BigDecimal]) =>
        if (ls.isDefinedAt(v1)) {
          val result = if(ls(v1) > 10) ls(v1) - 10 else ls(v1)
          Ok(
            Json.obj(
              "result" -> Json.obj("status" -> 200),
              "data" -> result
            )
          ).withToken
        } else {
          Ok(
            Json.obj(
              "result" -> Json.obj("status" -> 400),
              "msg" -> s"No number found in file2 at index $v1"
            )
          ).withToken
        }
      case Left(msg: String) =>
        Ok(
          Json.obj(
            "result" -> Json.obj("status" -> 400),
            "msg" -> msg
          )
        ).withToken
    }
  }

  def post(v2: Int, v3: Int, v4: Int) = Action.async { implicit request =>
    numbers(1) zip numbers(2) flatMap { nums =>
      if(nums._1.isRight && nums._2.isRight) {
        val firstList = nums._1.right.getOrElse(Nil)
        val secondList = nums._2.right.getOrElse(Nil)
        if (firstList.isDefinedAt(v3) && secondList.isDefinedAt(v4)) {
          val data = if (firstList(v3) + v2 < 10) (1,firstList(v3) + v2 + 10) else (2,firstList(v3) + v2)
          val ls = secondList.splitAt(v4)._1 ++ List(data._2) ++ secondList.splitAt(v4)._2
          save(ls) map {
            case Right(s) =>
              Ok(
                Json.obj(
                  "result" -> Json.obj("status" -> 200),
                  "data" -> data._1
                )
              ).withToken
            case Left(msg: String) =>
              Ok(
                Json.obj(
                  "result" -> Json.obj("status" -> 400),
                  "msg" -> msg
                )
              ).withToken
          }
        } else {
          Future.successful(
            Ok(
              Json.obj(
                "result" -> Json.obj("status" -> 400),
                "msg" ->
                  s"No number found in file${ if (firstList.isDefinedAt(v3)) 2 else 1 } at index ${ if (firstList.isDefinedAt(v3)) v4 else v3 }"
              )
            ).withToken
          )
        }
      } else {
        val msgs =
          List(
            nums._1.left.getOrElse(""),
            nums._2.left.getOrElse("")
          ).sortBy(_.isEmpty).mkString(";")
        Future.successful(
          Ok(
            Json.obj(
              "result" -> Json.obj("status" -> 400),
              "msg" -> msgs
            )
          ).withToken
        )
      }
    }
  }
}
