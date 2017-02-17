package in.flow.security

import java.io.{ByteArrayInputStream, InputStream}
import java.security.PublicKey
import javax.servlet.http.{HttpServletRequest, HttpServletRequestWrapper}
import javax.servlet.{ReadListener, ServletInputStream}

import org.json4s._
import org.scalatra.ActionResult
import org.scalatra.servlet.RichRequest

/**
  * Boilerplate for the server to decrypt / encrypt on the fly
  */

class DecryptedRichRequest(request: HttpServletRequest)(implicit security: Security)
  extends RichRequest(DecryptedRichRequest.wrapRequest(request, security)) {

  override def inputStream: InputStream = {
    println("\n\nGetting input stream of Rich request")
    DecryptedRichRequest.replacementStream(request, security)
  }
}

object DecryptedRichRequest {
  private def wrapRequest(implicit r: HttpServletRequest, s: Security): HttpServletRequestWrapper =
    new HttpServletRequestWrapper(r) {
    override def getInputStream: ServletInputStream = {
      println("Getting input stream of Wrapped request")
      new ServletInputStreamWrapper(replacementStream)
    }
  }
  private def replacementStream(implicit r: HttpServletRequest, s: Security): InputStream = {
    // if there is an error, then the input stream should be empty;
    // halting will be taken care of somewhere else
    val decrypted: Array[Byte] = ServletSecurity.receive.fold(_ => Array[Byte](), a => a)
    val replacement_stream = new ByteArrayInputStream(decrypted)
    replacement_stream
  }
}

class ServletInputStreamWrapper(stream: InputStream) extends ServletInputStream {
  override def isFinished: Boolean = stream.available() == 0

  override def isReady: Boolean = this.isFinished

  /** Hopefully this does not break things */
  override def setReadListener(readListener: ReadListener): Unit = throw new NotImplementedError()

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
    val enc = ServletSecurity.send(stringed)
    actionResult match {
      case ar: ActionResult => ar.copy(body = enc)
      case _ => enc
    }
  }

}