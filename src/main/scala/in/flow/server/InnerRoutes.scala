package in.flow.server

import akka.http.scaladsl.server.Directives.{as, complete, path, post}
import in.flow.security.Security
import in.flow.server.ServerDirectives
import in.flow.users.registration.{Registrar, RegistrationRequest, RegistrationResponse}

/**
  * Done mainly to simplify testing
  */
trait InnerRoutes extends JsonSupport {
  protected val sd: ServerDirectives = ServerDirectives

  def insecureInnerRoute(s: Security) = post {
    path("register") {
      sd.sentity(as[RegistrationRequest], s) {reg_req =>
        val reg_resp: RegistrationResponse = Registrar.registrationResultToResponse(Registrar.register(reg_req))
        complete(reg_resp)
      }
    }
  }
}
