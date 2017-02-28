package in.flow.users.registration

import com.wix.accord._
import in.flow.users.UserAccount

/**
  * Created by anton on 20/01/17.
  */
private[users] object Registrar {
  private def valid(message: RegistrationMessage): Boolean = {
    true
  }

  private def invitationCodeIsValid(code: String): Boolean = {
    true
  }

  def register(message: RegistrationMessage): Either[UserAccount, Failure] = {
    ???
  }
}

case class RegistrationMessage(invitation_code: String, name: String)