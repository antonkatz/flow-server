package in.flow.commformats

import java.sql.Timestamp
import java.time.Instant

import in.flow.commformats.ExternalCommFormats.{OfferResponse, TransactionResponse}
import in.flow.commformats.InternalCommFormats.TransactionType.TransactionType
import in.flow.users.{UserAccount, UserAccountPointer}

/**
  * Objects used for communication internally between and within modules
  */
object InternalCommFormats {

  object UserConnectionType extends Enumeration {
    type UserConnectionType = Value
    val creator, friend = Value
  }

  /* offers */

  trait OfferPointer {
    val offer_id: String
  }

  case class Offer(offer_id: String, from: UserAccountPointer, to: UserAccountPointer, hours: BigDecimal,
                   description: Option[String] = None) extends OfferPointer

  /* transaction and wallet */

  object TransactionType extends Enumeration {
    type TransactionType = Value
    val offer, interest = Value
  }

  case class UserWallet(owner: UserAccountPointer, transactions: Seq[Transaction],
                        committed_balance: Option[BigDecimal] = None)

  trait TransactionPointer {
    val transaction_id: String
  }

  object TransactionPointer {
    def apply(id: String): TransactionPointer = new TransactionPointer {
      val transaction_id = id
    }
  }

  trait Transaction extends TransactionPointer {
    val parent: TransactionPointer
    val from: UserAccountPointer
    val to: UserAccountPointer
    val amount: BigDecimal
    val timestamp: Instant
    val transaction_type: TransactionType
    val has_children: Boolean
  }

  case class OfferTransaction(transaction_id: String,
                              parent: TransactionPointer,
                              from: UserAccountPointer,
                              to: UserAccountPointer,
                              amount: BigDecimal,
                              timestamp: Instant,
                              has_children: Boolean,
                              offer: OfferPointer) extends Transaction {
    override val transaction_type: TransactionType = TransactionType.offer
  }
}
