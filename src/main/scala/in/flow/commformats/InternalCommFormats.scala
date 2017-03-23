package in.flow.commformats

import java.time.{Instant, ZonedDateTime}

import in.flow.commformats.ExternalCommFormats.{OfferResponse, TransactionResponse}
import in.flow.users.UserAccount

/**
  * Objects used for communication internally between and within modules
  */
object InternalCommFormats {

  /* todo incomplete*/
  type LazyOffer = OfferResponse

  type Transaction = TransactionResponse


  object UserConnectionType extends Enumeration {
    type UserConnectionType = Value
    val creator, friend = Value
  }

  object TransactionType extends Enumeration {
    type TransactionType = Value
    val offer, interest = Value
  }
}
