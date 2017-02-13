package in.flow.registration

import org.scalatra.commands._
import scala.util.control.Exception._
import org.scalatra.validation._
import scalaz._
import Scalaz._
import scala.util.control.Exception._
import org.scalatra.validation._
import org.scalatra.commands._

/**
  * Created by anton on 20/01/17.
  */
private[registration] object Registrar {
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