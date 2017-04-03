package in.flow.users

import java.sql.Timestamp
import java.util.Base64

import in.flow.algorithm.AlgorithmSettings
import in.flow.commformats.ExternalCommFormats.OfferRequest
import in.flow.commformats.InternalCommFormats.{Offer, OfferPointer}
import in.flow.db.OfferStatusType.OfferStatusType
import in.flow.db.{Db, DbSchema, OfferStatusType, OfferStorable}
import in.flow.{DatabaseError, FutureErrorFlow, InvalidInputError, UnknownError, WithErrorFlow, getLogger, getNow}
import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3
import scribe._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/**
  * Creating offers, accepting and rejecting
  * Checks for validity of offers made
  */
object Offers {
  private val logger: Logger = "Offers"

  private def getSha = new DigestSHA3(256)

  def createOffer(request: OfferRequest, creator: UserAccount): Future[WithErrorFlow[Offer]] = {
    Future(cleanAndValidate(request)) flowWith { r => checkPermissions(r, creator) } flowWith { r =>
      storeOffer(r, creator) flowRight {
        storableToOffer
      } recover {
        case e => logger.error(s"Failed to create an offer: ${e.getMessage}")
          Left(UnknownError())
      }
    }
  }

  /** returns all pending (open) offers to and from the given user */
  def getPendingOffers(user: UserAccount): Future[WithErrorFlow[Iterable[Offer]]] = {
    retrieveOffers(user, OfferStatusType.open) flowRight (_ map storableToOffer)
  }

  // todo must check that the user has autority to do so
  def retrieveOffer(offer: OfferPointer): FutureErrorFlow[Offer] = {
    Db.run(DbSchema.offers.filter(_.offerId === offer.offer_id).result) collect {
      case o if o.length == 1 => Right(storableToOffer(o.head))
    } recover {
      case e => logger.error(s"Failed to retrieve an offer: ${e.getMessage}")
        Left(DatabaseError("we could not load an offer with such id"))
    }
  }

  def completeOffer(offer: Offer): FutureErrorFlow[Offer] = {
    changeOfferStatus(offer.offer_id, OfferStatusType.completed) flowRight (_ => offer)
  }

  def rejectOffer(offer: Offer): FutureErrorFlow[Offer] = {
    changeOfferStatus(offer.offer_id, OfferStatusType.rejected) flowRight (_ => offer)
  }

  // todo must check that the user has authority to do so
  /**
    * when a transaction happens on an offer, it must be marked completed */
  private def changeOfferStatus(offer_id: String, status: OfferStatusType): FutureErrorFlow[String] = {
    val now = Timestamp.from(getNow)
    val q = for {o <- DbSchema.offers if o.offerId === offer_id} yield (o.status, o.timestamp_updated)
    Db.run(q.update(status.toString, now)) map { _ => Right(offer_id) } recover {
      case e => logger.error(s"Failed to update offer status: ${e.getMessage}")
        Left(DatabaseError("we couldn't update offer info, sorry"))
    }
  }

  /** @return all non-completed or non-rejected offers (in other words pending?) to and from user */
  private def retrieveOffers(u: UserAccount, status: OfferStatusType): Future[WithErrorFlow[Iterable[OfferStorable]]]
  = {
    Db.run(DbSchema.offers.filter(dbo => (dbo.to === u.user_id || dbo.from === u.user_id) && dbo.status === status
      .toString).result) map {
      Right(_)
    } recover {
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
    Users.loadUserConnections(creator) flatMap { u =>
      val has_connection = Future {
        u.connections exists (_._1.user_id == r.to_user_id)
      }
      // if the connection is not direct, check all
      has_connection collect {
        case false => Connections.getVisibleConnections(u) map { cons =>
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
  private def storableToOffer(o: OfferStorable): Offer = {
    val from = UserAccountPointer(o.from_user_id)
    val to = UserAccountPointer(o.to_user_id)
    val desc = if (o.description.nonEmpty) Option(o.description) else None
    Offer(o.offer_id, from = from, to = to, o.hours, desc)
  }

  /** creates an id for an offer */
  private def produceId(r: OfferRequest): String = {
    val produce_from: String = r.to_user_id + r.description.getOrElse("") + r.hours + getNow.toEpochMilli
    val id_byte = getSha.digest(produce_from.getBytes)
    Base64.getEncoder.encodeToString(id_byte)
  }

}
