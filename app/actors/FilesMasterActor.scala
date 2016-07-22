package actors

import akka.actor.{Props, Actor}
import javax.inject._
import play.api.{Environment, Configuration}
import scala.io.Source
import java.io._

case class GetNumberList(file: Int) {
  def filename = file match {
    case 1 => "f1.csv"
    case _ => "f2.csv"
  }
}
case class SaveNumberList(ls: List[BigDecimal])

class FilesMasterActor @Inject() (environment: Environment, configuration: Configuration) extends Actor {
  val filesFolder = configuration.getString("filesPath").getOrElse("files")

  def receive = {
    case get: GetNumberList =>
      environment.getExistingFile(s"$filesFolder/${get.filename}").map { file =>
        val bufferedSource = Source.fromFile(file)
        bufferedSource.getLines.toList.map { line =>
          line.split(",").map ( n => BigDecimal(n.trim)).toList
        }.headOption.map { ls =>
          sender ! Right(ls)
        } getOrElse {
          sender ! Left(s"Empty ${get.filename}")
        }
        bufferedSource.close
      } getOrElse {
        sender ! Left("Can't read f2.csv")
      }
    case SaveNumberList(ls) =>
      environment.getExistingFile(s"$filesFolder/${GetNumberList(2).filename}").map { file =>
        val pw = new PrintWriter(file)
        try {
          pw.write(ls.mkString(","))
          sender ! Right("")
        } catch {
          case e: IOException => sender ! Left(e.getMessage)
        } finally {
          pw.close
        }
      } getOrElse {
        sender ! Left("Can't read f2.csv")
      }
    case _ =>
  }
}