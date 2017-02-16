package in.flow.server

import java.io.{CharArrayWriter, PrintWriter}
import java.security.PublicKey
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import in.flow.security.{DecryptedServletRequest, Encryption, ServletEncryption}
import in.flow.users._
import in.flow.users.registration.RegisterCommand
import org.json4s.{DefaultFormats, Formats, JValue}
import org.scalatra._
import org.scalatra.commands._
import org.scalatra.json.JacksonJsonSupport
import org.scalatra.validation.ValidationError
import org.slf4j.LoggerFactory

import scalaz.NonEmptyList
import scalaz.Scalaz._

class FlowServlet extends FlowServerStack with JacksonJsonParsing with JacksonJsonSupport {
  private val log = LoggerFactory.getLogger("info")

  before() {
    contentType = formats("json")
  }

  get("/") {
    "jrebel"
  }

  post("/register") {
    log.info("attempting to register at /register")
    val cmd = command[RegisterCommand]
    log.info("with name {}, invitation code {}, public key {};",
      ~cmd.name.value, ~cmd.invitation_code.value, ~cmd.public_key.value)
    val cmd_result = cmd >> (RegisterCommand.performRegistration(_))
    cmd_result.fold(
      (errors: NonEmptyList[ValidationError]) => halt(400, errors.head),
      success => Ok(success)
    )
  }

  override def parsedBody(implicit request: HttpServletRequest): JValue = if (decrypt_incoming) {
    val modified_request = new DecryptedServletRequest(request)
    super.parsedBody(modified_request)
  } else super.parsedBody(request)

  override protected def renderResponseBody(actionResult: Any): Unit = {
    super.renderResponseBody(ServletEncryption.encrypt(actionResult, getSendersPublicKey))
  }

  protected[flow] def getSendersPublicKey: PublicKey = ???

  protected implicit lazy val jsonFormats: Formats = DefaultFormats
}