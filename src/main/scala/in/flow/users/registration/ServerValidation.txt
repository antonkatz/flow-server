package in.flow.users.registration

import org.json4s.{DefaultFormats, Formats}
import org.scalatra.commands._
import org.scalatra.validation.ValidationError

import scala.language.implicitConversions
import scalaz.Scalaz._
import scalaz._

/**
  * Created by anton on 20/01/17.
  */
class RegisterCommand extends JsonCommand {
  val name: Field[String] = asType[String]("name").notBlank
  val invitation_code: Field[String] = asType[String]("invitation_code").notBlank
  val public_key: Field[String] = asType[String]("public_key").notBlank

  override protected implicit def jsonFormats: Formats = DefaultFormats

}

object RegisterCommand {
  implicit def toRegistrationMessage(cmd: RegisterCommand): RegistrationMessage =
    RegistrationMessage(~cmd.invitation_code.value, ~cmd.name.value)

  def performRegistration(cmd: RegisterCommand): ModelValidation[Null] = {
    Registrar.register(cmd) match {
      case true => Validation.success(null)
      case false => ValidationError("could not register for unknown reason").failureNel
    }
  }
}

