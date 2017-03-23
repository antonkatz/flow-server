package in.flow.commformats

import java.time.Instant

import in.flow.users.UserAccount

import scala.collection.Iterable

/**
  * Created by anton on 22/03/17.
  */
object ExternalCommFormats {

  case class RegistrationRequest(invitation_code: String, display_name: String)

  case class RegistrationResponse(id: String)

  /** @param fof friends of friends */
  case class ConnectionsResponse(friends: Iterable[UserRepresentation], fof: Seq[Iterable[UserRepresentation]])

  case class UserRepresentation(user_id: String, display_name: String)

  /* offers
   * todo. hours should be a big decimal */

  /** for creating an offer */
  case class OfferRequest(to_user_id: String, hours: BigDecimal, description: Option[String])

  case class OfferActionRequest(offer_id: String)

  case class OfferResponse(offer_id: String, from_user_id: String, to_user_id: String, hours: BigDecimal, description:
  Option[String] = None)
  {
    def withDescription(description: String): OfferResponse = {
      val d = if (description.isEmpty) None else Option(description)
      this.copy(description = d)
    }
  }

  case class OffersResponse(offers: Iterable[OfferResponse])

  /* wallet and transactions */

  /** timestamp is UTC millis since epoch */
  case class TransactionResponse(transaction_id: String, from_user_id: String, to_user_id: String,
                                 amount:BigDecimal, timestamp: Long, offer_id: Option[String])

}
