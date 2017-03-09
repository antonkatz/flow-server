package in.flow.users.registration

import java.util.concurrent.TimeUnit

import com.wix.accord._
import in.flow.db.{Db, DbSchema}
import in.flow.users.{UserAccount, Users}
import org.slf4j.LoggerFactory
import slick.dbio.DBIOAction

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.{Random, Success}

//import in.flow.db.Ptm._
import slick.jdbc.PostgresProfile.api._
import scala.concurrent.duration._
/**
  * Created by anton on 20/01/17.
  */
object Registrar {
  type Response[T] = Either[(String, String), T]

  private val logger = LoggerFactory.getLogger("Registrar")

  /** how many words to combine to create a code */
  private val invitaiton_code_size = 3
  /** words used to create codes */
  private val word_list = io.Source.fromFile("common-words.txt").getLines().map(_.trim).toSeq
  private val word_list_size = word_list.length

  /*
  *  creating invitations
  * */
  def createInvitation(from: UserAccount): Response[Invitation] = {
    createInvitationCode() match {
      case None => Left("database_error", "Could not create invitation")
      case Some(code_words) =>
        val i = Invitation(from, code_words)
        val df = Db.db.run(DbSchema.invitations += i.storable)
        try {
          Await.ready(df, 1 second)
          Right(i)
        } catch {
          case _: Throwable => Left(("database_error", "Could not create an invitation"))
        }
    }
  }

  @tailrec
  private def createInvitationCode(): Option[Seq[String]] = {
    val words = (1 to invitaiton_code_size) map (_ => Random.nextInt(word_list_size)) map {word_list(_)}
    checkCodeExists(words) match {
      case Some(true) => createInvitationCode()
      case Some(false) => Some(words)
      case None => None
    }
  }

  private def checkCodeExists(ws: Seq[String]): Option[Boolean] = {
    val q = DbSchema.invitations.filter(_.code === makeCode(ws)).exists.result
    try {
      val r = Await.result(Db.run(q), 1 second)
      Some(r)
    } catch {
      case _:Throwable => None
    }
  }

  private[registration] def makeCode(ws: Seq[String]) = ws.mkString("-")
  /*
  *  registering
  * */

  def register(message: RegistrationRequest): Either[Failure, UserAccount] = {
    logger.info("an attempt to register is made with {}", message.toString)
    Right(UserAccount("test_id"))
  }

  private def valid(message: RegistrationRequest): Boolean = {
    true
  }

  private def invitationCodeIsValid(code: String): Boolean = {
    true
  }

  implicit def registrationResultToResponse(res: Either[Failure, UserAccount]): RegistrationResponse = {
    res fold (
      fail => {
        val v = fail.violations.headOption
        RegistrationResponse(None, v.map(_.constraint), v.map(v => Descriptions.render(v.description)))
      },
      ua => RegistrationResponse(Some(ua.user_id))
    )

  }
}

case class RegistrationRequest(invitation_code: String, desired_name: String)

case class RegistrationResponse(id: Option[String],
                                error_code: Option[String] = None, error_msg: Option[String] = None)

case class Invitation(from: UserAccount, code_words: Seq[String]) {
  def code = Registrar.makeCode(code_words)
  def storable = InvitationStored(from.user_id, code)
}

case class InvitationStored(user_id: String, code: String)