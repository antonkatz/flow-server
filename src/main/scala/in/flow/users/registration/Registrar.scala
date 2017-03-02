package in.flow.users.registration

import com.wix.accord._
import in.flow.users.{UserAccount, Users}

import org.slf4j.LoggerFactory


/**
  * Created by anton on 20/01/17.
  */
object Registrar {
  private val logger = LoggerFactory.getLogger("Registrar")

  private def valid(message: RegistrationRequest): Boolean = {
    true
  }

  private def invitationCodeIsValid(code: String): Boolean = {
    true
  }

  def register(message: RegistrationRequest): Either[Failure, UserAccount] = {
    logger.info("an attempt to register is made with {}", message.toString)
    Right(UserAccount("test_id"))
  }

  implicit def registrationResultToResponse(res: Either[Failure, UserAccount]): RegistrationResponse = {
    res fold (
      fail => {
        val v = fail.violations.headOption
        RegistrationResponse(None, v.map(_.constraint), v.map(v => Descriptions.render(v.description)))
      },
      ua => RegistrationResponse(Some(ua.id))
    )

  }
}

case class RegistrationRequest(invitation_code: String, desired_name: String)

case class RegistrationResponse(id: Option[String],
                                error_code: Option[String] = None, error_msg: Option[String] = None)