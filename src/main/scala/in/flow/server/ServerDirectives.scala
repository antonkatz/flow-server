package in.flow.server

import java.security.GeneralSecurityException
import java.util.Base64
import java.util.concurrent.TimeUnit

import akka.http.scaladsl.model.HttpEntity.Strict
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.`Access-Control-Expose-Headers`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RejectionHandler, _}
import akka.http.scaladsl.server.directives.RouteDirectives.reject
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import in.flow.security.{MissingUser, NeedAsymmetricKey, NeedSymmetricKey, Security}
import in.flow.users.{UserAccount, Users}
import org.bouncycastle.util.encoders.Hex
import scribe._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * akka-http modified directives to suit our needs
  */
trait ServerDirectives {
  private val logger = "Server Directives".logger

  private val symmetric_key_header_name = "sym_key"

  private def secureResponseFlow(implicit s: Security): Flow[ByteString, ByteString, _] = Flow.fromFunction { (old) =>
    Security.sendPayload(old.utf8String) match {
      case Left(e) => ByteString()
      /*todo. THIS IS TERRIBLE. PERHAPS THERE WILL BE A BETTER WAY THROUGH SINKS*/
      case Right(msg) =>
        ByteString(Hex.toHexString(msg.iv ++ msg.message))
    }
  }

  /** de-formats the data (assumes hex, base64 if header is present) */
  private def secureRequestFlow(format: String)(implicit s: Security): Flow[ByteString, ByteString, _] = Flow.fromFunction { old =>
    val deformatted = format match {
      case "base64" => Base64.getDecoder.decode(old.toArray)
      case _ => Hex.decode(old.toArray)
    }
    val res = Security.receivePayload(deformatted)
    res match {
      case Left(er) => ByteString()
      case Right(msg) => ByteString(msg)
    }
  }

  /** returns the error message and the [[StatusCode]] */
  private def generateErrorResponse(er: Throwable): (String, StatusCode) = er match {
    case _: NeedAsymmetricKey => val m = "Missing public key"
      (m, StatusCodes.custom(412, reason = m))
    case _: NeedSymmetricKey => val m = "Missing symmetric key"
      (m, StatusCodes.custom(421, reason = m))
    case _: MissingUser => val m = "Could not find user"
      (m, StatusCodes.custom(401, reason = m))
    case _: GeneralSecurityException => ("Error during encryption or decryption of traffic. Cause unknown.", 500)
    case _ => ("Unknown error. Assuming server fault", 500)
  }

  def securityDirective: Directive[Tuple1[Security]] = (optionalHeaderValueByName("user_id") &
    optionalHeaderValueByName("iv") & optionalHeaderValueByName("public_key")) tmap {
    case (user_id: Option[String], iv: Option[String], public_key: Option[String]) =>

      val user = user_id flatMap {id =>
        Await.result(Users.getUser(id), Duration.create(2, TimeUnit.SECONDS))
      }

      val iv_bytes = iv map Hex.decode
      implicit val s = Security(user, iv_bytes)

      public_key map { k => Security setPublicKey b64(k) }

      s
  }

  /** setting a public key if one has been given, which should come double base64 encoded to preserve newlines */
  def b64(s: String) = new String(Base64.getDecoder.decode(s), in.flow.global_string_format)


  def secureRequestDirective(implicit security: Security): Directive0 = mapRequest { r =>
    val format = r.getHeader("format") match {
      case o if o.isPresent => o.get().value().trim
      case _ => "hex"
    }
    r.mapEntity { e =>
      e.transformDataBytes(secureRequestFlow(format))
    }
  } trequire (predicate = { _ =>
    Security.provideError.isEmpty
  })

  def secureResponseDirective(implicit security: Security): Directive0 = mapResponse { response =>
    var r = response
    // if there's an error, there is no point encrypting the response; it is discarded
    if (Security.provideError.isEmpty) {
      r = response mapEntity { entity =>
        entity.transformDataBytes(secureResponseFlow)
      }
    }
    // adds symmetric key, and again checks for errors
    mapResponseWithPotentialError(r)
  }

  /** always adds symmetric key if needed, and checks for errors (through security), generating appropriate response */
  private def mapResponseWithPotentialError(_r: HttpResponse)(implicit s: Security) = {
    var r = _r
    r = addSymmetricKey(r)

    Security.provideError match {
      case Some(e) =>
        val (ermsg, code) = generateErrorResponse(e)
        r.copy(status = code, entity = Strict(ContentTypes.`text/plain(UTF-8)`, ByteString(ermsg)))
      case _ => r
    }
  }

  private def addSymmetricKey(r: HttpResponse)(implicit s: Security) = {
    {Security.sendSymmetricKey collect { case key =>
      headerPartial(r)(HttpHeader.parse(symmetric_key_header_name, Hex.toHexString(key)))
    } getOrElse r}
      // for pesky browsers
      .addHeader(`Access-Control-Expose-Headers`(symmetric_key_header_name))
  }

  def sentity[T](um: FromRequestUnmarshaller[T], s: Security): Directive1[T] = {
    ConjunctionMagnet.fromDirective[Tuple1[T], Unit]({
      Directive { inner =>
        Security.provideError(s) match {
          case Some(er) => reject(SecurityRejection(er))
          case None => inner(())
        }
      }
    }).apply(entity(um))
  }

  def securityRejectionHandler(implicit s: Security): Directive0 = {
    val handler = RejectionHandler.newBuilder().handle {
      case SecurityRejection(_) => complete(mapResponseWithPotentialError(HttpResponse()))
    }.result()
    handleRejections(handler)
  }

  case class SecurityRejection(er: Throwable) extends Rejection

  /**
    * @throws MatchError
    * removes some boilerplate code with adding headers
    * given a header parsing result and a response, returns the response with the header */
  def headerPartial(r: HttpResponse): PartialFunction[ParsingResult, HttpResponse] = {
    case ParsingResult.Ok(h, _) => r.mapHeaders(_ :+ h)
  }
}

object ServerDirectives extends ServerDirectives