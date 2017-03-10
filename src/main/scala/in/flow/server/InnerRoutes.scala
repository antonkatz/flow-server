package in.flow.server

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import in.flow.security.Security
import in.flow.users.registration.{Registrar, RegistrationRequest, RegistrationResponse}
import org.slf4j.LoggerFactory
import in.flow.{MissingPublicKeyError, ServerError, UserError}

/**
  * Done mainly to simplify testing
  */
trait InnerRoutes extends JsonSupport {
  private val logger = LoggerFactory.getLogger("Inner Routes")

  protected val sd: ServerDirectives = ServerDirectives

  def insecureInnerRoute(implicit s: Security) = post {
    path("register") {
      logger.info("attempting to register")
      sd.sentity(as[RegistrationRequest], s) {reg_req =>
        val reg_result = Registrar.register(reg_req, Security.getPublicKey)
        val resp: FlowResponse = reg_result
        val status: StatusCode = reg_result match {
          case Right(_) => StatusCodes.OK
          case Left(e: ServerError) => StatusCodes.InternalServerError
          case Left(e: MissingPublicKeyError) => StatusCodes.custom(412, reason = e.message)
          case Left(_: UserError) => StatusCodes.BadRequest
        }

        complete(status, resp)
      }
    }
  }
}
