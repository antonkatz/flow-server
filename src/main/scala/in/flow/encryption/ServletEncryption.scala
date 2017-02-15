package in.flow.encryption

import java.io.{ByteArrayInputStream, InputStream}
import javax.servlet.{ReadListener, ServletInputStream}
import javax.servlet.http.{HttpServletRequest, HttpServletRequestWrapper}

/**
  * Boilerplate for the server to decrypt / encrypt on the fly
  */

class DecryptedRequest(request: HttpServletRequest) extends HttpServletRequestWrapper(request) {
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
