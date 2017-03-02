package in.flow.server

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import in.flow.security.Security
import in.flow.server.ServerDirectives
import in.flow.users.registration.{Registrar, RegistrationRequest, RegistrationResponse}
import org.slf4j.LoggerFactory

/**
  * Done mainly to simplify testing
  */
trait InnerRoutes extends JsonSupport {
  private val logger = LoggerFactory.getLogger("Inner Routes")

  protected val sd: ServerDirectives = ServerDirectives

  def insecureInnerRoute(s: Security) = post {
    path("register") {
      logger.info("attempting to register")
      sd.sentity(as[RegistrationRequest], s) {reg_req =>
        val reg_resp: RegistrationResponse = Registrar.registrationResultToResponse(Registrar.register(reg_req))
        complete(reg_resp)
      }
    }
  }
}
