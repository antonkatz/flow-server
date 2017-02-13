package in.flow.server

import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64
import javax.servlet.{ReadListener, ServletInputStream}
import javax.servlet.http.{HttpServletRequest, HttpServletRequestWrapper}

import in.flow.encryption.Encryption
import in.flow.registration._
import org.scalatra._
import org.json4s.{DefaultFormats, Formats, JValue}
import org.scalatra.json.{JacksonJsonOutput, JacksonJsonSupport}
import org.slf4j.LoggerFactory
import org.scalatra.commands._
import org.scalatra.json.JsonSupport._
import org.scalatra.scalate.ScalateSupport
import org.scalatra.validation.ValidationError

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

  override def parsedBody(implicit request: HttpServletRequest): JValue = {
    val modified_request = new DecryptedRequest(request)
    super.parsedBody(modified_request)
  }

  protected implicit lazy val jsonFormats: Formats = DefaultFormats
}

class DecryptedRequest(request: HttpServletRequest) extends HttpServletRequestWrapper(request) {
  override def getInputStream: ServletInputStream = {
    val original_stream = io.Source.fromInputStream(request.getInputStream())
    val payload = original_stream.getLines().mkString
    println("original payload: " + payload)
    val decrypted = Encryption.receive(payload)
    println("decrypted: " + decrypted)
    val replacement_stream = new ByteArrayInputStream(decrypted.getBytes)
    new ServletInputStreamWrapper(replacement_stream)
  }
}

class ServletInputStreamWrapper(stream: InputStream) extends ServletInputStream {
  override def isFinished: Boolean = stream.available() == 0

  override def isReady: Boolean = this.isFinished

  override def setReadListener(readListener: ReadListener): Unit = {}

  override def read(): Int = stream.read()
}