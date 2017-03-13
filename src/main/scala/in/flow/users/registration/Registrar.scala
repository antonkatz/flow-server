package in.flow.users.registration

import java.security.PublicKey
import java.util.Base64

import in.flow.{DatabaseError, FlowError, InvalidInputError, MissingPublicKeyError, UnknownError => UEr}
import com.wix.accord._
import in.flow.db.{Db, DbSchema}
import in.flow.users.UserAccount
import com.wix.accord.dsl._

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.language.{implicitConversions, postfixOps}
import scala.util.Random
import scala.util.control.Exception._
import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scribe._
import _root_.in.flow._
import _root_.in.flow.server.FlowResponse
/**
  * Created by anton on 20/01/17.
  */
object Registrar {
  private val logger: Logger = "Registrar"

  /** how many words to combine to create a code */
  private val invitaiton_code_size = 3
  /** words used to create codes */
  private val word_list = io.Source.fromFile("common-words.txt").getLines().map(_.trim.toLowerCase).toSeq
  private val word_list_size = word_list.length

  /*
  *  creating invitations
  * */
  private val ie = DatabaseError("we could not create an invitation")

  def createInvitation(from: UserAccount): FlowResponseType[Invitation] = {
    createInvitationCode() match {
      case None => Left(ie)
      case Some(code_words) =>
        val i = Invitation(from, code_words)
        val df = Db.db.run(DbSchema.invitations += i.storable)
        try {
          Await.ready(df, 1 second)
          Right(i)
        } catch {
          case _: Throwable => Left(ie)
        }
    }
  }

  @tailrec
  private def createInvitationCode(): Option[Seq[String]] = {
    val words = (1 to invitaiton_code_size) map (_ => Random.nextInt(word_list_size)) map {
      word_list(_)
    }
    checkCodeExists(words) match {
      case Some(true) => createInvitationCode()
      case Some(false) => Some(words)
      case None => None
    }
  }

  private def checkCodeExists(ws: Seq[String]): Option[Boolean] = {
    checkCodeExists(makeCode(ws))
  }

  private def invitationCodeQuery(code: String) = DbSchema.invitations.filter(_.code === code)

  private def checkCodeExists(code: String): Option[Boolean] = {
    val q = invitationCodeQuery(code).exists.result
    try {
      val r = Await.result(Db.run(q), 1 second)
      Some(r)
    } catch {
      case _: Throwable => None
    }
  }

  private[registration] def makeCode(ws: Seq[String]) = ws.mkString(" ")

  /*
  *  registering
  * */

  implicit private val requestValidator: Validator[RegistrationRequest] =
    validator[RegistrationRequest] { m =>
      m.display_name as "name" is notEmpty
      m.invitation_code as "invitation code" is notEmpty
    }

  /** validates the message, checks that the invitation code exists in the database, and then creates and stores a
    * new user account
    *
    * @param public_key [[None]] results in an error
    **/
  def register(req: RegistrationRequest, public_key: Option[PublicKey]): FlowResponseType[UserAccount] = {
    logger.info(s"an attempt to register is made with $req")
    public_key map { pk =>
      validate(req) match {
        case f: Failure =>
          logger.warn("the registration message was improperly formatted")
          failureToResponse(f)
        case Success =>
          val ic = req.invitation_code.trim.toLowerCase
          invitationCodeIsValid(ic) match {
            case None => Left(DatabaseError("we don't know if the invitation code is valid"))
            case Some(false) => Left(InvalidInputError("the invitation code is invalid"))
            case Some(true) => registerUnsafe(ic, req.display_name.trim, pk)
          }
      }
    } getOrElse Left(MissingPublicKeyError())
  }

  def isRegistered(public_key: Option[PublicKey]): FlowResponseType[Option[UserAccount]] = public_key map { pk =>
    getUserId(pk) map {id =>
      allCatch[Option[UserAccount]] either {
        val accounts = Await.result(Db.run(DbSchema.user_accounts.filter(_.id === id).result), 1 second)
        accounts.headOption
      } fold (e => {
        logger.warn(s"Unexpected error during a registration check: ${e.getMessage}")
        Left(UEr().asInstanceOf[FlowError])
      }, s => Right(s))
    } joinRight
  } getOrElse Left(MissingPublicKeyError())

  private def getUserId(public_key: PublicKey): FlowResponseType[String] = {
    allCatch[String].either({
      val id_byte = new DigestSHA3(256).digest(public_key.getEncoded)
      Base64.getEncoder.encodeToString(id_byte)
    }).fold(e => {
      logger.warn(s"Failed to generate id with error: ${e.getMessage}")
      Left(UEr())
    }, id => Right(id))
  }

  /** removes the invitation code, creates a new user account; expects all arguments to be "proper" (clean) */
  private def registerUnsafe(ic: String, dn: String, public_key: PublicKey): FlowResponseType[UserAccount] = {
    val res = getUserId(public_key).right.map(id => {
      val u = UserAccount(id, dn)
      allCatch.either({
        // store user, and delete invitation code
        val uins: Int = Await.result(Db.run(DbSchema.user_accounts += u), 1 second)
        if (uins == 1) {
          Db.run(DbSchema.invitations.filter(_.code === ic).delete).onComplete(_.recover {
            case e => logger.error(s"Was not able to delete the invitation code: ${e.getMessage}")
          })
          Right(u)
        } else Left(DatabaseError("we couldn't create a user account"))
      }) fold(e => {
        logger.error(s"Failed to store a new user: ${e.getMessage}")
        Left(UEr())
      }, _ => {
        logger.info("registration appears successful")
        Right(u)
      })
    })

    res.joinRight
  }

  private def invitationCodeIsValid(code: String): Option[Boolean] = checkCodeExists(code)

  private def failureToResponse(f: Failure) = {
    val v = f.violations.headOption
    Left(InvalidInputError(v.map(v => Descriptions.render(v.description)).getOrElse("")))
  }
}

case class RegistrationRequest(invitation_code: String, display_name: String)

case class RegistrationResponse(id: String)

case class Invitation(from: UserAccount, code_words: Seq[String]) {
  def code = Registrar.makeCode(code_words)

  def storable = InvitationStored(from.user_id, code)
}

case class InvitationStored(user_id: String, code: String)