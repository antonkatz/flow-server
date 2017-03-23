package in.flow.users

import java.sql.Timestamp
import java.time.{LocalDateTime, ZonedDateTime}
import java.util.Base64

import in.flow.algorithm.AlgorithmSettings
import in.flow.commformats.ExternalCommFormats.{OfferRequest, OfferResponse, OffersResponse}
import in.flow.commformats.InternalCommFormats.LazyOffer
import in.flow.users.UserAccount
import in.flow.db.OfferStatusType.OfferStatusType
import in.flow.db.{Db, DbSchema, OfferStatusType, OfferStorable}
import in.flow.{DatabaseError, FutureErrorFlow, InvalidInputError, UnknownError, WithErrorFlow, getLogger, getNow}
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
      storeOffer(r, creator) flowRight { storableToOfferResponse } recover {
        case e => logger.error(s"Failed to create an offer: ${e.getMessage}")
          Left(UnknownError())
      }
    }
  }

  /** returns all pending (open) offers to the given user */
  def getPendingOffersTo(user: UserAccount): Future[WithErrorFlow[OffersResponse]] = {
    retrieveOffers(user, OfferStatusType.open) flowRight (_ map storableToOfferResponse) flowRight OffersResponse
  }

  // todo must check that the user has autority to do so
  def retrieveOffer(offer_id: String): FutureErrorFlow[LazyOffer] = {
    Db.run(DbSchema.offers.filter(_.offerId === offer_id).result) collect {
      case o if o.length == 1 => Right(storableToOfferResponse(o.head))
    } recover {
      case e => logger.error(s"Failed to retrieve an offer: ${e.getMessage}")
        Left(DatabaseError("we could not load an offer with such id"))
    }
  }

  def completeOffer(offer: LazyOffer): FutureErrorFlow[LazyOffer] = {
    changeOfferStatus(offer.offer_id, OfferStatusType.completed) flowRight(_ => offer)
  }

  def rejectOffer(offer_id: String): FutureErrorFlow[String] = {
    changeOfferStatus(offer_id, OfferStatusType.rejected)
  }

  // todo must check that the user has authority to do so
  /**
    * when a transaction happens on an offer, it must be marked completed */
  private def changeOfferStatus(offer_id: String, status: OfferStatusType): FutureErrorFlow[String] = {
    val now = Timestamp.from(getNow)
    val q = for {o <- DbSchema.offers if o.offerId === offer_id } yield (o.status, o.timestamp_updated)
    Db.run(q.update(status.toString, now)) map {_ => Right(offer_id)} recover {
      case e => logger.error(s"Failed to update offer status: ${e.getMessage}")
        Left(DatabaseError("we couldn't update offer info, sorry"))
    }
  }

  /** @return all non-completed or non-rejected offers (in other words pending?) */
  private def retrieveOffers(u: UserAccount, status: OfferStatusType): Future[WithErrorFlow[Iterable[OfferStorable]]]
  = {
    Db.run(DbSchema.offers.filter(dbo => dbo.to === u.user_id && dbo.status === status.toString).result) map {
      Right(_)} recover {
      case e => logger.error(s"Failed to retrieve an offer: ${e.getMessage}")
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
    // todo .can big decimal fail us on this condition
    val non_divisible_units = clean.hours % AlgorithmSettings.transaction_amount_lower_bound
    if (non_divisible_units != 0)
      return Left(InvalidInputError("you are splitting hairs"))
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
    val now = Timestamp.from(getNow)
    OfferStorable(id, from_user_id = creator.user_id, to_user_id = r.to_user_id, hours = r.hours,
      r.description.getOrElse(""), timestamp_created = now,
      OfferStatusType.open, timestamp_updated = now)
  }

  /** parses stored version into a response version */
  private def storableToOfferResponse(o: OfferStorable): OfferResponse = {
    OfferResponse(o.offer_id, o.from_user_id, to_user_id = o.to_user_id, o.hours) withDescription o.description
  }

  /** creates an id for an offer */
  private def produceId(r: OfferRequest): String = {
    val produce_from: String = r.to_user_id + r.description.getOrElse("") + r.hours + LocalDateTime.now().getNano
    val id_byte = sha.digest(produce_from.getBytes)
    Base64.getEncoder.encodeToString(id_byte)
  }

}
