package in.flow.server

import java.io.{CharArrayWriter, InputStream, PrintWriter}
import java.security.PublicKey
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import in.flow.security._
import in.flow.users._
import in.flow.users.registration.RegisterCommand
import org.json4s.{DefaultFormats, Formats, JValue}
import org.scalatra._
import org.scalatra.commands._
import org.scalatra.json.JacksonJsonSupport
import org.scalatra.servlet.RichRequest
import org.scalatra.validation.ValidationError
import org.slf4j.LoggerFactory

import scala.language.implicitConversions
import scalaz.NonEmptyList
import scalaz.Scalaz._

class FlowServlet extends FlowServerStack with JacksonJsonParsing with JacksonJsonSupport {
  private val log = LoggerFactory.getLogger("Servlet /v1")

  private[this] implicit var security: Security = _

  before() {
    contentType = formats("json")
    security = ServletSecurity.apply
    Security.provideError map {_ => halt(401, "Most likely cause is decryption error")}
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

  /** wrapping the request to decrypt it without the servlet needing to know */
  override implicit def enrichRequest(request: HttpServletRequest): RichRequest = new DecryptedRichRequest(request)

  override protected def renderResponseBody(actionResult: Any): Unit = {
    super.renderResponseBody(ServletSecurity.send(actionResult))
  }

  protected[flow] def getSendersPublicKey: PublicKey = Encryption.getServerPublicKey

  protected implicit lazy val jsonFormats: Formats = DefaultFormats
}