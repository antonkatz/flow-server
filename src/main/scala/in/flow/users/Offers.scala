package in.flow.users

import java.time.LocalDateTime
import java.util.Base64

import in.flow.commformats.{OfferRequest, OfferResponse, OffersResponse}
import in.flow.db.{Db, DbSchema, OfferStorable}
import in.flow.{DatabaseError, InvalidInputError, UnknownError, WithErrorFlow, getLogger}
import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3
import scribe._

import scala.concurrent.Future
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Creating offers, accepting and rejecting
  * Checks for validity of offers made
  */
object Offers {
  private val logger: Logger = "Offers"
  private val sha = new DigestSHA3(256)

  def createOffer(request: OfferRequest, creator: UserAccount): Future[WithErrorFlow[OfferResponse]] = {
    Future(cleanAndValidate(request)) flowWith { r => checkPermissions(r, creator)} flowWith { r =>
      storeOffer(r, creator) flowRight { stored =>
        OfferResponse(stored.offer_id, stored.from_user_id, stored.hours) withDescription stored.description
      } recover {
        case e => logger.error(s"Failed to create an offer: ${e.getMessage}")
          Left(UnknownError())
      }
    }
  }

  def getOffersTo(user: UserAccount): Future[WithErrorFlow[OffersResponse]] = {
    retrieveOffers(user) flowRight (_ map storableToOfferResponse) flowRight OffersResponse
  }

  /** @return all non-completed or non-rejected offers (in other words pending?) */
  private def retrieveOffers(u: UserAccount): Future[WithErrorFlow[Iterable[OfferStorable]]] = {
    Db.run(DbSchema.offers.filter(_.to === u.user_id).result) map {Right(_)} recover {
      case e => logger.error(s"Failed to store an offer: ${e.getMessage}")
        Left(DatabaseError("we messed up loading offers to you, please report this error"))
    }
  }

  /** @return full stored info */
  private def storeOffer(r: OfferRequest, creator: UserAccount): Future[WithErrorFlow[OfferStorable]] = {
    val storable = offerRequestToStorable(r, creator)
    Db.run((DbSchema.offers returning DbSchema.offers) += storable) map (Right(_)) recover {
      case e => logger.error(s"Failed to store an offer: ${e.getMessage}")
        Left(DatabaseError("we messed up, please report this error"))
    }
  }

  /** cleans up the values in `r` and checks for validity of data */
  private def cleanAndValidate(r: OfferRequest): WithErrorFlow[OfferRequest] = {
    val clean = r.copy(r.to_user_id.trim, r.hours, r.description.map(_.trim))
    if (clean.hours <= 0) return Left(InvalidInputError("you have to give more than nothing"))
    if (clean.to_user_id.isEmpty) return Left(InvalidInputError("whom are you giving this time to?"))
    Right(clean)
  }

  /** checks that the two users are indeed connected */
  private def checkPermissions(r: OfferRequest, creator: UserAccount): Future[WithErrorFlow[OfferRequest]] = {
    Users.loadUserConnections(creator) flatMap {u =>
      val has_connection = Future{u.connections exists(_._1.user_id == r.to_user_id)}
      // if the connection is not direct, check all
      has_connection collect {
        case false => Connections.getVisibleConnections(u) map {cons =>
            val flat_cons = cons.flatten
            if (flat_cons.exists(_.user_id == r.to_user_id)) Right(r)
            else Left(InvalidInputError("umm... you don't know this person"))
          }

        case true => Future(Right(r))
      } flatten
    }
  }

  /** parses the request into a storable version */
  private def offerRequestToStorable(r: OfferRequest, creator: UserAccount) = {
    val id = produceId(r)
    OfferStorable(id, from_user_id = creator.user_id, to_user_id = r.to_user_id, hours = r.hours,
      r.description.getOrElse(""))
  }

  /** parses stored version into a response version */
  private def storableToOfferResponse(o: OfferStorable): OfferResponse = {
    OfferResponse(o.offer_id, o.from_user_id, o.hours) withDescription o.description
  }

  /** creates an id for an offer */
  private def produceId(r: OfferRequest): String = {
    val produce_from: String = r.to_user_id + r.description.getOrElse("") + r.hours + LocalDateTime.now().getNano
    val id_byte = sha.digest(produce_from.getBytes)
    Base64.getEncoder.encodeToString(id_byte)
  }

}
