package in.flow.users.registration

/**
  * Created by anton on 20/01/17.
  */
private[users] object Registrar {
  def valid(message: RegistrationMessage): Boolean = {
    true
  }

  def invitationCodeIsValid(code: String): Boolean = {
    true
  }

  def register(message: RegistrationMessage): Boolean = {
    true
  }
}

case class RegistrationMessage(invitation_code: String, name: String)