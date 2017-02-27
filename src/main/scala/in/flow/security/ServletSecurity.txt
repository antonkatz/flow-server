package in.flow.security

import java.util.Base64
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import in.flow.users.{User, Users}
import in.flow.utils.Hex
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.{compact, render}
import org.scalatra.{ActionResult, ResponseStatus}

/**
  * Functions dealing with the servlet, which should be separate from the code that does not need to come into contact
  * with servlets, but are not direct encryption
  */
object ServletSecurity {
  def apply(request: HttpServletRequest): Security = {
    implicit val r = request
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

  /** Takes a servlet action (that could just be a string or [[JValue]]), and returns either an action with encrypted
    * body and added headers */
  def send(servlet_action: Any)(implicit r: HttpServletResponse, s: Security): ActionResult = {
    /* the first time there must be a cipher sent, encrypted with the public key of the user */
    val payload = actionExtract(servlet_action)
    Security.sendPayload(payload).fold({ e =>
      // in case of error. this action result will get overwritten
      ActionResult(ResponseStatus(401), "", headers = Map())
    }, { encrypted_payload: SymmetricallyEncrypted =>
      val esk = Security.sendSymmetricKey
      val symmetric_key = esk map {
        Hex.parse
      }
      var headers: Map[String, String] = Map("iv" -> Hex.parse(encrypted_payload.iv))
      if (symmetric_key.nonEmpty) headers += ("key" -> symmetric_key.get)

      servlet_action match {
        case ar: ActionResult => ar.copy(body = encrypted_payload, headers = ar.headers ++ headers)
        case _ => ActionResult(ResponseStatus(200), encrypted_payload.message, headers = headers)
      }
    })
  }


  private def actionExtract(action: Any): String = {
    val to_string = action match {
      case ActionResult(_, body, _) => body
      case other => other
    }
    actionBodyCompact(to_string)
  }

  private def actionBodyCompact: PartialFunction[Any, String] = {
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

