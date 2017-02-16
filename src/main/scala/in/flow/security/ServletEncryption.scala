package in.flow.security

import java.io.{ByteArrayInputStream, InputStream}
import java.security.PublicKey
import javax.servlet.{ReadListener, ServletInputStream}
import javax.servlet.http.{HttpServletRequest, HttpServletRequestWrapper}

import org.json4s._
import org.scalatra.ActionResult

/**
  * Boilerplate for the server to decrypt / encrypt on the fly
  */

class DecryptedServletRequest(request: HttpServletRequest) extends HttpServletRequestWrapper(request) {
  override def getInputStream: ServletInputStream = {
    val original_stream = io.Source.fromInputStream(request.getInputStream())
    val payload = original_stream.getLines().mkString
    val decrypted = Encryption.receive(payload)
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

/** For encryption of the body that is sent back to the user */
object ServletEncryption {
  import org.json4s.jackson.JsonMethods._

  /** This method breaks some of the functionality of Scalatra, but none of the important functions (that's the hope) */
  def encrypt(actionResult: Any, senders_public_key: PublicKey): Any = {
    val to_string = actionResult match {
      case ActionResult(_, body, _) => body
      case other => other
    }
    val stringed = stringify(to_string)
    val enc = Encryption.send(stringed, senders_public_key)
    actionResult match {
      case ar: ActionResult => ar.copy(body = enc)
      case _ => enc
    }
  }

  private def stringify: PartialFunction[Any, String] = {
    case _: Unit | Unit | null => ""
    case ar: JValue => compact(render(ar))
    case ar: String => ar
    case other => other.toString
  }
}