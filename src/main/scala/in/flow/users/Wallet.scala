package in.flow.users

import java.sql.Timestamp
import java.time.Instant
import java.util.Base64

import in.flow.algorithm.Accounting
import slick.jdbc.PostgresProfile.api._
import in.flow.{DatabaseError, FutureErrorFlow, WithErrorFlow, _}
import in.flow.commformats.InternalCommFormats.{Transaction, UserWallet}
import in.flow.db.{Db, DbSchema, TransactionStorable}

import scala.concurrent.ExecutionContext.Implicits.global
import in.flow.commformats.ExternalCommFormats.TransactionResponse
import in.flow.commformats.InternalCommFormats.TransactionType
import in.flow.commformats.InternalCommFormats._
import in.flow.commformats.InternalCommFormats.TransactionType.TransactionType
import in.flow.users.UserAccount
import scribe.Logger

//todo. the current transaction process must be reimplemented to resemble BitCoin
/**
  * Responsible for all transactions interactions, balances, and financial interest operations
  *
  */
object Wallet {
  private val logger: Logger = "Wallet"

  type TransactionResult = FutureErrorFlow[Transaction]

  /** performs a transaction, checking that the user is allowed to do so
    * WARNING, currently does not check didly squat */
  def createTransaction(from: UserAccount, to: UserAccount, amount: BigDecimal): TransactionResult = {
    throw new NotImplementedError("creating transaction only from offers at this moment")
  }

  /** performs a transaction, checking that the user is allowed to do so
    * WARNING, currently does not check didly squat */
  def createTransaction(offer: Offer): FutureErrorFlow[OfferTransaction] = {
    val now = getNow
    val gen_id_from = offer.from.user_id + offer.to.user_id + offer.hours.toString() + now
    val t_id_bytes = global_sha.digest(gen_id_from.getBytes(global_string_format))
    val t_id = Base64.getEncoder.encodeToString(t_id_bytes)
    val t = OfferTransaction(t_id, from = offer.from, to = offer.to,
      amount = offer.hours, now, offer = offer)

    Offers.completeOffer(offer) flowWith {_ => store(t)}
  }

  /** @see [[Accounting.loadCommittedBalance()]]*/
  def loadAuxWalletInfo(wallet: UserWallet): UserWallet = {
    Accounting.loadCommittedBalance(wallet)
  }

  /** HOT. The database access can fail giving wrong balances
    * @return all transaction amounts summed up */
  def getWallet(user: UserAccountPointer): FutureErrorFlow[UserWallet] = {
    val q = DbSchema.transactions.filter(t => t.to === user.user_id || t.from === user.user_id).result

    Db.run(q) map {dbtrs =>
      val trs = dbtrs map fromStorable
      if (trs.exists(_.isEmpty)) {
        Left(DatabaseError("we messed up, sorry"))
      } else {
        Right(UserWallet(user, trs.flatten))
      }
    } recover {
      case e => logger.error(s"Failed during database access when getting wallet balance for user ${user.user_id}: " +
        s"${e.getMessage}")
        Left(DatabaseError("we couldn't get your balance"))
    }
  }

  private def store[T <: Transaction](t: T): FutureErrorFlow[T] = {
    val storable = asStorable(t)
    Db.run(DbSchema.transactions += storable) map {_ => Right(t)} recover {
      case e => logger.error(s"Could not store transaction: ${e.getMessage}")
        Left(DatabaseError("we couldn't store this transaction"))
    }
  }

  /** @return instance that can be put into a database */
  private def asStorable(t: Transaction): TransactionStorable = {
    val time = Timestamp.from(t.timestamp)
    val st = TransactionStorable(t.transaction_id, from_user_id = t.from.user_id, to_user_id = t.to.user_id,
      amount = t.amount, timestamp = time, offer_id = None, transaction_type = t.transaction_type.toString)
    t match {
      case t: OfferTransaction => st.copy(offer_id = Option(t.offer.offer_id))
      case _ => st
    }
  }

  /** HOT. does not match all types of transactions */
  private def fromStorable(t: TransactionStorable): Option[Transaction] = {
    val from = UserAccountPointer(t.from_user_id)
    val to = UserAccountPointer(t.to_user_id)
    TransactionType.withName(t.transaction_type) match {
      case tt if tt == TransactionType.offer => t.offer_id map {oid =>
        val offer = new OfferPointer {
          override val offer_id: String = oid
        }
        OfferTransaction(t.transaction_id, from=from, to=to, t.amount, t.timestamp.toInstant, offer)
      }
    }
  }

}
