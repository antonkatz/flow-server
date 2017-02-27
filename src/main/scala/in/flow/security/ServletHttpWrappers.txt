package in.flow.security

import java.io.{ByteArrayInputStream, InputStream}
import javax.servlet.http.{HttpServletRequest, HttpServletRequestWrapper}
import javax.servlet.{ReadListener, ServletInputStream}

import org.scalatra.servlet.RichRequest

/**
  * Boilerplate for the server to decrypt / encrypt on the fly
  */

//class DecryptedRichRequest(request: HttpServletRequest)(implicit security: Security)
//  extends RichRequest(DecryptedRichRequest.wrapRequest(request, security))

object DecryptedRichRequest {
  def wrapRequest(rf: () => HttpServletRequest, sf: () => Security): HttpServletRequestWrapper = {
    println("WRAPPING REQUEST & GENERATING STREAM")
    val (r, s) = (rf(), sf())
    val rep_stream = new ServletInputStreamWrapper(replacementStream(r, s))

    new HttpServletRequestWrapper(r) {
      override def getInputStream: ServletInputStream = {
        println("Getting input stream of Wrapped request")
        rep_stream
      }
    }
  }

  private def replacementStream(implicit r: HttpServletRequest, s: Security): InputStream = {
    // if there is an error, then the input stream should be empty;
    // halting will be taken care of somewhere else
    println("GEN REP STREAM")
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