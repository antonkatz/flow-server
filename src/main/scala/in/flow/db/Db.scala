package in.flow.db

/**
  * Created by anton on 06/03/17.
  */

import org.postgresql.util.PSQLException
import scribe._
import slick.dbio.DBIOAction
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Db {
  private val logger = "Db".logger
  private lazy val file = scala.io.Source.fromFile("settings")
  private lazy val settings = file.getLines().toSeq

  private val db: slick.jdbc.JdbcBackend.DatabaseDef = Database.forURL(getUrl + ";DB_CLOSE_DELAY=-1",
    driver = "org.postgresql.Driver")

  private val schema_creation_future = db.run(DBIOAction.seq(
    DbSchema.user_accounts.schema.create, DbSchema.invitations.schema.create,
    DbSchema.user_account_connections.schema.create,
    DbSchema.offers.schema.create,
    DbSchema.transactions.schema.create
  )) recover {
    case e: PSQLException if e.getSQLState == "42P07" =>
      logger.info(s"Schema already exists; as indicated by error: ${e.getMessage}")
    case e =>
      logger.info(s"Unexpected database error ${e.getMessage}")
      throw e
  }

  /** makes sure that the [[DBIOAction]] is ran after the creation of the schema */
  def run[R](a: DBIOAction[R, NoStream, Nothing]): Future[R] = schema_creation_future flatMap { _ => db.run(a) }

  /*
  * Line 1 comment
  * Line 2 db username
  * Line 3 db pass
  * */

  def getDbUsername = settings.lift(1).getOrElse("")

  def getDbPass = settings.lift(2).getOrElse("")

  def getUrl = "jdbc:postgresql:flow?user=" + getDbUsername + "&password=" + getDbPass
}