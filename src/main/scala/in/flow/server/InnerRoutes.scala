package in.flow.server

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import in.flow.algorithm.AccountingRules
import in.flow.commformats.ExternalCommFormats._
import in.flow.commformats.InternalCommFormats._
import in.flow.security.Security
import in.flow.users.{Wallet, _}
import in.flow.users.registration.Registrar
import in.flow.{MissingPublicKeyError, MissingUserError, ServerError, UserError, WithErrorFlow, _}
import scribe._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions

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
      sd.sentity(as[RegistrationRequest], s) { reg_req =>
        val reg_result = Registrar.register(reg_req, Security.getPublicKey)
        val resp: FlowResponse = toFlowResponse(reg_result)
        val status: StatusCode = reg_result

        complete(status, resp)
      }
    } ~ path("handshake") {
      logger.debug("checking if registered")
      val res: Option[RegistrationResponse] = Security.getOrLoadUser map { u => RegistrationResponse(u.user_id) }
      val resp: FlowResponse = toFlowResponse(res)(regResp.write)
      Security.getOrLoadUser foreach { _ => Security.refreshSymmetricKey }
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
        sd.sentity(as[Seq[String]], s) { requested_ids =>
          val user = Security.getOrLoadUser
          val res: Future[WithErrorFlow[Set[(String, String)]]] = user map { u =>
            Connections.resolveIdsToNames(requested_ids, u)
          } getOrElse {
            Future {
              Left(MissingUserError())
            }
          }
          val response: Future[(StatusCode, FlowResponse)] = res map { r =>
            getStatusCode(r) -> r
          }
          complete(response)
        }
      } ~ path("get-balances") {
        // todo. fix this is hacky
        logger.debug(s"retrieving balances of connections of user ${Security.getUserId.getOrElse("[missing id]")}")
        val consf: Future[Set[UserAccount]] =
          Security.getOrLoadUser map Connections.getVisibleConnectionsSet getOrElse Future(Set())
        val res: Future[Set[(String, BigDecimal)]] = consf flatMap { cons =>
          val futures: Set[Future[(String, BigDecimal)]] = cons map { c =>
            val w = Wallet.getWallet(c) flowRight Wallet.loadAuxWalletInfo
            val b: Future[BigDecimal] = w flowRight {_.balance getOrElse BigDecimal(0)} map {_.right getOrElse
              BigDecimal(0)}
            b map {c.user_id -> _}
          }
          Future.sequence(futures)
        }
        complete(res)
      }

    } ~ pathPrefix("offers") {
      logger.debug("accessing offers")
      path("create") {
        logger.debug(s"${Security.getUserId.getOrElse("[missing id]")} is creating an offer")
        sd.sentity(as[OfferRequest], s) { offer_req =>
          val user = Security.getOrLoadUser
          val res: Future[WithErrorFlow[Offer]] = user map { u => Offers.createOffer(offer_req, u) } getOrElse {
            Future {
              Left(MissingUserError())
            }
          }
          val response: Future[(StatusCode, FlowResponse)] = res map { r => getStatusCode(r) -> r.map(offerToResponse) }
          complete(response)
        }
      } ~ path("get") {
        logger.debug(s"getting offers for ${Security.getUserId.getOrElse("[missing id]")}")
        val user = Security.getOrLoadUser
        val res: Future[WithErrorFlow[Iterable[Offer]]] = user map { u => Offers.getPendingOffers(u) } getOrElse {
          Future {
            Left(MissingUserError())
          }
        }
        val response: Future[(StatusCode, FlowResponse)] = res map { r => getStatusCode(r) -> r.map(offersToResponse) }
        complete(response)
      } ~ path("get-completed") {
        logger.debug(s"getting offers for ${Security.getUserId.getOrElse("[missing id]")}")
        val user = Security.getOrLoadUser
        val res: Future[WithErrorFlow[Iterable[Offer]]] = user map { u => Offers.getCompletedOffers(u) } getOrElse {
          Future {
            Left(MissingUserError())
          }
        }
        val response: Future[(StatusCode, FlowResponse)] = res map { r => getStatusCode(r) -> r.map(offersToResponse) }
        complete(response)
      } ~ path("complete") {
        sd.sentity(as[OfferActionRequest], s) { req =>
          logger.debug(s"accepting offer ${req.offer_id} for ${Security.getUserId.getOrElse("[missing id]")}")
          val offer = Offers.retrieveOffer(req)
          val transaction = offer flowWith Users.performTransaction
          val resp: Future[(StatusCode, FlowResponse)] = transaction map { r =>
            getStatusCode(r) -> r.map(_ map transactionToResponse)
          }
          complete(resp)
        }
      } ~ path("reject") {
        sd.sentity(as[OfferActionRequest], s) { req =>
          logger.debug(s"rejecting offer ${req.offer_id} for ${Security.getUserId.getOrElse("[missing id]")}")
          val offer = Offers.retrieveOffer(req)
          val resp: Future[(StatusCode, FlowResponse)] = offer flowWith Offers.rejectOffer map { r =>
            getStatusCode(r) -> r.map(offerToResponse)
          }
          complete(resp)
        }
      } ~ path("cancel") {
        sd.sentity(as[OfferActionRequest], s) { req =>
          logger.debug(s"canceling offer ${req.offer_id} for ${Security.getUserId.getOrElse("[missing id]")}")
          val offer = Offers.retrieveOffer(req)
          val resp: Future[(StatusCode, FlowResponse)] = offer flowWith Offers.cancelOffer map { r =>
            getStatusCode(r) -> r.map(offerToResponse)
          }
          complete(resp)
        }
      }
    } ~ pathPrefix("wallet") {
      path("get") {
        val user = Security.getOrLoadUser toRight MissingUserError()
        val wallet = Future(user) flowWith { u => Wallet.getWallet(u) } flowRight {
          Wallet.loadAuxWalletInfo
        } flowRight {
          walletToResponse
        }
        val response: Future[(StatusCode, FlowResponse)] = wallet map { fe =>
          getStatusCode(fe) -> fe
        }
        complete(response)
      }
    } ~ pathPrefix("algorithm") {
      path("interest") {
        sd.sentity(as[TimeUnitRequest], s) { time_unit =>
          val i = AccountingRules.getPerTimeInterestRate(time_unit.time_unit)
          val resp = FlowResponse.success(BigDecimalJsonFormat.write(i))
          complete(resp)
        }
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
