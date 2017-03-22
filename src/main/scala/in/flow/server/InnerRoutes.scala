package in.flow.server

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import in.flow.MissingUserError
import in.flow.commformats._
import in.flow.security.Security
import in.flow.users.{Connections, Offers, UserAccount, Users}
import in.flow.users.registration.Registrar
import in.flow.{MissingPublicKeyError, ServerError, UnknownError, UserError, WithErrorFlow}
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

  // todo. maybe use both post and get?
  def insecureInnerRoute(implicit s: Security) = post {
    path("register") {
      logger.debug("attempting to register")
      sd.sentity(as[RegistrationRequest], s) {reg_req =>
        val reg_result = Registrar.register(reg_req, Security.getPublicKey)
        val resp: FlowResponse = toFlowResponse(reg_result)
        val status: StatusCode = reg_result

        complete(status, resp)
      }
    } ~ path("is_registered") {
      logger.debug("checking if registered")
      val res: Option[RegistrationResponse] = Security.getOrLoadUser map { u => RegistrationResponse(u.user_id)}
      val resp: FlowResponse = toFlowResponse(res)(regResp.write)
      Security.getOrLoadUser foreach { _ => Security.refreshSymmetricKey}
      complete(resp)

    } ~ pathPrefix("connections") {
      path("get") {
        logger.debug(s"retrieving connections of user ${Security.getUserId.getOrElse("[missing id]")}")
        val res: Future[Seq[Set[UserAccount]]] = Security.getOrLoadUser map Connections.getVisibleConnections getOrElse
          Future(Seq())
        complete(connectionsToFlowResponse(res))

      } ~ path("resolve-to-name") {
        logger.debug(s"attempting to resolve names of connections of " +
          s"user ${Security.getUserId.getOrElse("[missing id]")}")
        sd.sentity(as[Seq[String]], s) {requested_ids =>
          val user = Security.getOrLoadUser
          val res: Future[WithErrorFlow[Seq[String]]] = user map {u =>
            Connections.resolveIdsToNames(requested_ids, u)
          } getOrElse {
            Future {Left(MissingUserError())}
          }
          val response: Future[(StatusCode, FlowResponse)] = res map {r =>
            getStatusCode(r) -> r}
          complete(response)
        }
      }

    } ~ pathPrefix("offers") {
      logger.debug("accessing offers")
      path("create") {
        logger.debug(s"${Security.getUserId.getOrElse("[missing id]")} is creating an offer")
        sd.sentity(as[OfferRequest], s) {offer_req =>
          val user = Security.getOrLoadUser
          val res: Future[WithErrorFlow[OfferResponse]] = user map {u => Offers.createOffer(offer_req, u)} getOrElse {
            Future {Left(MissingUserError())}
          }
          val response: Future[(StatusCode, FlowResponse)] = res map {r => getStatusCode(r) -> (r:FlowResponse)}
          complete(response)
        }
      } ~ path("get") {
        logger.debug(s"getting offers for ${Security.getUserId.getOrElse("[missing id]")}")
        val user = Security.getOrLoadUser
        val res: Future[WithErrorFlow[OffersResponse]] = user map {u => Offers.getOffersTo(u)} getOrElse {
          Future {Left(MissingUserError())}
        }
        val response: Future[(StatusCode, FlowResponse)] = res map {r => getStatusCode(r) -> (r:FlowResponse)}
        complete(response)
      }
    }
  }

  implicit private def getStatusCode(response: WithErrorFlow[_]): StatusCode = response match {
    case Right(_) => StatusCodes.OK
    case Left(e: ServerError) => StatusCodes.InternalServerError
    case Left(e: MissingPublicKeyError) => StatusCodes.custom(412, reason = e.message)
    case Left(_: UserError) => StatusCodes.BadRequest
    // should never be invoked
    case _ => StatusCodes.InternalServerError
  }
}
