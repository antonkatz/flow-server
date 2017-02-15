package in.flow.server

import java.io.{CharArrayWriter, PrintWriter}
import java.security.PublicKey
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import in.flow.encryption.{DecryptedRequest, Encryption}
import in.flow.registration._
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

  override def parsedBody(implicit request: HttpServletRequest): JValue = if (production) {
    val modified_request = new DecryptedRequest(request)
    super.parsedBody(modified_request)
  } else super.parsedBody(request)

  override protected def renderResponseBody(actionResult: Any): Unit = {
    def stringify: PartialFunction[Any, String] = {
      case _: Unit | Unit | null => ""
      case ar: JValue => compact(render(ar))
      case ar: String => ar
      case other => other.toString
    }
    val to_string = actionResult match {
      case ActionResult(_, body, _) => body
      case other => other
    }
    val stringed = stringify(to_string)
    val enc = Encryption.send(stringed, getSendersPublicKey)
    val modified_result = actionResult match {
      case ar: ActionResult => ar.copy(body = enc)
      case _ => enc
    }
    super.renderResponseBody(modified_result)
  }

  protected[flow] def getSendersPublicKey: PublicKey = ???

  protected implicit lazy val jsonFormats: Formats = DefaultFormats
}

private case class EncryptedJsonResponse(response: String)

import javax.servlet.http.HttpServletResponseWrapper

class ResponseWrapper(response: HttpServletResponse) extends HttpServletResponseWrapper(response) {
  private val output = new CharArrayWriter()

  override def toString(): String = {
    output.toString()
  }

  override def getWriter(): PrintWriter = {
    new PrintWriter(output)
  }
}