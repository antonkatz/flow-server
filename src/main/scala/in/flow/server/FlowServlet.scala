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
import in.flow.security.NeedSymmetricKey

class FlowServlet extends FlowServerStack with JacksonJsonParsing with JacksonJsonSupport {
  protected val log = LoggerFactory.getLogger("Servlet /v1")

  private var security_object: Security = _
  implicit def security = security_object

  override def handle(req: HttpServletRequest, res: HttpServletResponse): Unit = {
    security_object = ServletSecurity.apply(req)
    super.handle(req, res)
  }

  before() {
    contentType = formats("json")
    Security.provideError map {
      case _: NeedSymmetricKey => halt(421, "The server no longer has the symmetric key")
      case _ => halt(401, "Most likely cause is decryption error")
    }
  }

  after() {
    Security.provideError map {
      case _: NeedAsymmetricKey =>
        halt(412, "The server does not have a viable public key for the user")
      case _ =>
        halt(401, "Most likely cause is encryption error")
    }
  }

  get("/") {
      "jrebel"
  }

  post("/register") {
    log.info("attempting to register at /register")
    val cmd = command[RegisterCommand]
    log.info("with name {}, invitation code {}, public key {};",
      ~cmd.name.value, ~cmd.invitation_code.value, ~cmd.public_key.value)

    Security.setPublicKey(~cmd.public_key.value)

    val cmd_result = cmd >> (RegisterCommand.performRegistration(_))
    cmd_result.fold(
      (errors: NonEmptyList[ValidationError]) => halt(400, errors.head),
      success => {
        Ok(success)
      }
    )
  }

  override def post(transformers: RouteTransformer*)(action: => Any): Route = {
    lazy val wrapped_request = DecryptedRichRequest.wrapRequest({() => request}, {() => security})
    lazy val wrapped_action = withRequest(wrapped_request) {action}
    super.post(transformers:_*)(wrapped_action)
  }

  /** wrapping the request to decrypt it without the servlet needing to know */
//  override implicit def enrichRequest(request: HttpServletRequest): RichRequest = new DecryptedRichRequest(request)

  override protected def renderResponseBody(actionResult: Any): Unit = {
    super.renderResponseBody(ServletSecurity.send(actionResult))
  }

  protected[flow] def getSendersPublicKey: PublicKey = Encryption.getServerPublicKey

  protected implicit lazy val jsonFormats: Formats = DefaultFormats
}