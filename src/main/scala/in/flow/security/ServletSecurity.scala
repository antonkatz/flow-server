package in.flow.security

import java.util.Base64
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import in.flow.users.{User, Users}
import in.flow.utils.Hex
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.{compact, render}

/**
  * Functions dealing with the servlet, which should be separate from the code that does not need to come into contact
  * with servlets, but are not direct encryption
  */
object ServletSecurity {
  def apply(implicit request: HttpServletRequest): Security = {
    val user = parseUser
    /** initialization vector; if present means that the body is symmetrically encrypted */
    val iv = parseIv
    Security(user, iv)
  }

  def receive(implicit r: HttpServletRequest, s: Security): Either[Throwable, Array[Byte]] = {
    val payload = Stream.continually(r.getInputStream.read()).takeWhile(_ != -1).map(_.toByte).toArray
    // payload could be base64 encoded
    val parsed_payload = Option(r.getHeader("encoding_format")) match {
      case Some("base64") => Base64.getDecoder.decode(payload)
      case _ => payload
    }
    Security.receivePayload(parsed_payload)
  }

  def send(payload: Any)(implicit r: HttpServletResponse, s: Security): Unit = {
    /* the first time there must be a cipher sent, encrypted with the public key of the user */

    val encrypted_payload = ServletEncryption.encrypt(payload)
  }


  private def stringify: PartialFunction[Any, String] = {
    case _: Unit | Unit | null => ""
    case ar: JValue => compact(render(ar))
    case ar: String => ar
    case other => other.toString
  }

  private def parseUser(implicit r: HttpServletRequest): Option[User] = {
    Option(r.getHeader("user_id")) flatMap { id => Users.getUser(id) }
  }

  private def parseIv(implicit r: HttpServletRequest): Option[Array[Byte]] = {
    Option(r.getHeader("iv")) map {
      Hex.parse
    }
  }
}

