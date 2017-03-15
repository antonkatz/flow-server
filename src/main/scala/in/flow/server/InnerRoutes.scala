package in.flow.server

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import in.flow.security.Security
import in.flow.users.{UserAccount, Users}
import in.flow.users.registration.{Registrar, RegistrationRequest, RegistrationResponse}
import in.flow.{FlowResponseType, MissingPublicKeyError, ServerError, UserError}
import spray.json.JsValue
import scribe._

import scala.concurrent.{Future, Promise}
import scala.language.implicitConversions
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Done mainly to simplify testing
  */
trait InnerRoutes extends JsonSupport {

  private val logger = "Inner Routes".logger

  protected val sd: ServerDirectives = ServerDirectives

  def insecureInnerRoute(implicit s: Security) = post {
    path("register") {
      logger.info("attempting to register")
      sd.sentity(as[RegistrationRequest], s) {reg_req =>
        val reg_result = Registrar.register(reg_req, Security.getPublicKey)
        val resp: FlowResponse = toFlowResponse(reg_result)
        val status: StatusCode = reg_result

        complete(status, resp)
      }
    } ~ path("is_registered") {
      logger.info("checking if registered")
      val res = Registrar.isRegistered(Security.getPublicKey)
      val resp: FlowResponse = toFlowResponse(res)

      val p = Promise[Unit]()
      val f = p.future;
      // todo. double lookup
      {Security.getPublicKey flatMap Users.getUserId}.fold[Any](p.success(Unit))(id => Users.lazyGetUser(id) map {u =>
        u foreach {u => Security.setUser(u); Security.refreshSymmetricKey}
      } onComplete {_ => p success Unit})

      val fr: Future[FlowResponse] = f map {_ => resp}
      complete(res: StatusCode, fr)
    } ~ path("get_connections") {
      logger.info(s"retrieving connections of user ${Security.getUserId.getOrElse("[missing id]")}")
      val res = Seq(Set("u1"), Set("u2"))
      complete(Future(res))
    }
  }

  implicit def userToRegResp(ua: UserAccount): Option[JsValue] = Option(regResp write RegistrationResponse(ua.user_id))
  implicit def optUserIdToRegResp(uid: Option[String]): Option[JsValue] = uid map { uid =>
    regResp write RegistrationResponse(uid)}

  implicit private def getStatusCode(response: FlowResponseType[_]): StatusCode = response match {
    case Right(_) => StatusCodes.OK
    case Left(e: ServerError) => StatusCodes.InternalServerError
    case Left(e: MissingPublicKeyError) => StatusCodes.custom(412, reason = e.message)
    case Left(_: UserError) => StatusCodes.BadRequest
    // should never be invoked
    case _ => StatusCodes.InternalServerError
  }
}
