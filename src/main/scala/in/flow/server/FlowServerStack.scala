package in.flow.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import in.flow.security.Security
import in.flow.users.Users
import org.bouncycastle.util.encoders.Hex

import scala.io.StdIn

object FlowServerStack {

  import ServerDirectives._

  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher


    val route = securityDirective { implicit security =>
      Security.setPublicKey(PubKeyTest.k)
      secureResponseDirective(security) {
        path("hello") {
          get {
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
          }
        }
      }
    }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}

private[server] object ServerDirectives {
  def secureResponseFlow(implicit s: Security): Flow[ByteString, ByteString, _] = Flow.fromFunction { (old) =>
    Security.sendPayload(old.utf8String) match {
      case Left(e) => ByteString()
      /*todo. THIS IS TERRIBLE. PERHAPS THERE WILL BE A BETTER WAY THROUGH SINKS*/
      case Right(msg) =>
        ByteString(msg.iv) ++ ByteString(msg.message)
    }
  }

  def secureRequestFlow(implicit s: Security): Flow[ByteString, ByteString, _] = Flow.fromFunction { old =>
    Security.receivePayload(old.toArray) match {
      case Left(er) => ByteString()
      case Right(msg) => ByteString(msg)
    }
  }

  def errorFlow(msg: String): Flow[ByteString, ByteString, _] = Flow.fromFunction { old => ByteString(msg) }

  /** returns the error message and the [[StatusCode]] */
  def generateErrorResponse(er: Throwable): (String, StatusCode) = er match {
    // TODO implement
    case _ => ("error", 400)
  }

  def securityDirective = (optionalHeaderValueByName("user_id") & optionalHeaderValueByName("iv")) tmap {
    case (user_id, iv) =>
      val user = user_id flatMap Users.getUser
      val iv_bytes = iv map Hex.decode
      Security(user, iv_bytes)
  }

  def secureRequestDirective(implicit security: Security) = mapRequest { r =>
    r.mapEntity { e =>
      e.transformDataBytes(secureRequestFlow)
    }
  } & {
    Security.provideError match {
      case Some(_) => reject
      case _ => pass
    }
  }

  def secureResponseDirective(implicit security: Security) = mapResponse { response =>
    def headerPartial(r: HttpResponse): PartialFunction[ParsingResult, HttpResponse] = {
      case ParsingResult.Ok(h, _) => r.withHeaders(h)
    }

    var r = response
    // if there's an error, there is no point encrypting the response; it is discarded
    if (Security.provideError.isEmpty) {
      r = response mapEntity { entity =>
        entity.transformDataBytes(secureResponseFlow)
      }
    }
    r = Security.sendSymmetricKey collect { case key =>
      headerPartial(r)(HttpHeader.parse("key", Hex.toHexString(key)))
    } getOrElse r

    // in case of error
    Security.provideError match {
      case Some(er) => val (ermsg, code) = generateErrorResponse(er)
        r mapEntity {
          _ transformDataBytes errorFlow(ermsg)
        } copy (status = code)
      case _ => r
    }
  }
}

object PubKeyTest {
  val k = "-----BEGIN PUBLIC KEY-----\nMIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAvq7+LdGjlYwk4y8iov2x\ngSZElxrFNbpkJguDJVFwJDFh+GcXZOENOQ8LOEKAHFq9pLm5hxZspjE9EajT9SlD\nmLu89Vq82r1Rls5crPG3c2eIPlb3+IBG22WG63Sbl71AKYk90CXXmfmCfEkydacz\nnSBpzuoUGpt7VUH5XIe4NRPT6oJqcK7GN2ZO5csguMhj+5vcNIWC3+x3oCEMrm1W\nB4ifU8CU4Tdf0tZ/n2rVCe0H/F42wBAgyH3Nfv3XPMpo2h9CbUpmaANARS0gIXSb\nrSrPwAPhrQIElJ0eWU/YAqLVjdczr6PPdiQYo1HLhb8IZFU9eanYRC+HGHjooq0A\nOaHbPUywzxNHwmmC/Eh17PdBIvwJe0rTy4JLDz8mDz5mQpbeB8hMCyOk9mHFu8cJ\nW0digUadNTZMFHkMSq6pGGfdAzH7CGf7IZ5oXYt1wGVBqO2VRx2zOuoSh5cPyQoz\nuzzJTOGCXCzOBvUXc25RvAhcHCokLVkEWOY/p+sYRmBn8IOsUXqjW437WUUsCRwQ\nOLXe0VrofdvCtwmIEk4oriSGo6zrbFC3vqf0GmQuDpdjprM3dnXevfDWejAGWl06\ndBHLTGQx6kWNGkxKw5mDikVF+f/6+8g8nVetNEH34qzVduPk9+LY09T0dxaGuXoS\nRPCMi9MxK/SUFEPiiW02R18CAwEAAQ==\n-----END PUBLIC KEY-----"
}