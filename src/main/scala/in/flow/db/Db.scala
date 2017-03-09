package in.flow.db

/**
  * Created by anton on 06/03/17.
  */

import slick.dbio.DBIOAction
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object Db {
  private lazy val file = scala.io.Source.fromFile("settings")
  private lazy val settings = file.getLines().toSeq

  val db: slick.jdbc.JdbcBackend.DatabaseDef = Database.forURL(getUrl + ";DB_CLOSE_DELAY=-1",
    driver = "org.postgresql.Driver")

  Await.ready(db.run(DBIOAction.seq(
    (DbSchema.user_accounts.schema ++ DbSchema.invitations.schema).create
  )), Duration.Inf)

  def run[R](a: DBIOAction[R, NoStream, Nothing]): Future[R] = db.run(a)

  /*
  * Line 1 comment
  * Line 2 db username
  * Line 3 db pass
  * */

  def getDbUsername = settings.lift(1).getOrElse("")

  def getDbPass = settings.lift(2).getOrElse("")

  def getUrl = "jdbc:postgresql:flow?user=" + getDbUsername + "&password=" + getDbPass
}