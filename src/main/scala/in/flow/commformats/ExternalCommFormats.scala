package in.flow.commformats

import java.time.Instant

import in.flow.commformats.InternalCommFormats._
import in.flow.users.{UserAccount, UserAccountPointer}

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

  /* offers */

  /** for creating an offer */
  case class OfferRequest(to_user_id: String, hours: BigDecimal, description: Option[String])

  case class OfferActionRequest(offer_id: String) extends OfferPointer

  case class OfferResponse(offer_id: String, from_user_id: String, to_user_id: String, hours: BigDecimal, description:
  Option[String] = None) extends OfferPointer
  {
    def withDescription(description: String): OfferResponse = {
      val d = if (description.isEmpty) None else Option(description)
      this.copy(description = d)
    }
  }

  case class OffersResponse(offers: Iterable[OfferResponse])

  implicit def offerToResponse(o: Offer): OfferResponse = {
    OfferResponse(o.offer_id, from_user_id = o.from.user_id, to_user_id = o.to.user_id, o.hours, o.description)
  }

  implicit def offersToResponse(os: Iterable[Offer]) = OffersResponse(os map offerToResponse)

  /* wallet and transactions */

  /** timestamp is UTC millis since epoch */
  case class TransactionResponse(transaction_id: String, from_user_id: String, to_user_id: String,
                                 amount:BigDecimal, timestamp: Long, offer_id: Option[String])

  case class WalletResponse(principal: BigDecimal, interest: BigDecimal, uncommitted_interest: BigDecimal,
                            transactions:Iterable[TransactionResponse])

  implicit def transactionToResponse(t: Transaction): TransactionResponse = {
    val tr = TransactionResponse(t.transaction_id, from_user_id = t.from.user_id, to_user_id = t.to.user_id,
      amount = t.amount, t.timestamp.toEpochMilli, None)
    t match {
      case t: OfferTransaction => tr.copy(offer_id = Option(t.offer.offer_id))
      case _ => tr
    }
  }

  def walletToResponse(w: UserWallet): WalletResponse = {
    val trs = {w.transactions sortBy (_.timestamp) reverse} map transactionToResponse
    val p = w.principal getOrElse 0:BigDecimal
    val i = w.interest getOrElse 0:BigDecimal
    val ui = w.uncommitted_interest getOrElse 0:BigDecimal
    WalletResponse(p, i, ui, trs)
  }

  /* algorithm */

  case class TimeUnitRequest(time_unit: Int)
}
