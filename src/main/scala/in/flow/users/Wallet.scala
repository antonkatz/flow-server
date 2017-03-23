package in.flow.users

import java.sql.Timestamp
import java.time.Instant
import java.util.Base64

import slick.jdbc.PostgresProfile.api._
import in.flow.{FutureErrorFlow, WithErrorFlow}
import in.flow.commformats.InternalCommFormats.{LazyOffer, Transaction}
import in.flow.db.{Db, DbSchema, TransactionStorable}

import scala.concurrent.ExecutionContext.Implicits.global
import in.flow._
import in.flow.commformats.ExternalCommFormats.TransactionResponse
import in.flow.commformats.InternalCommFormats.TransactionType
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
  def createTransaction(offer: LazyOffer): TransactionResult = {
    val now = getNow.toEpochMilli
    val gen_id_from = offer.from_user_id + offer.to_user_id + offer.hours.toString() + now
    val t_id_bytes = global_sha.digest(gen_id_from.getBytes(global_string_format))
    val t_id = Base64.getEncoder.encodeToString(t_id_bytes)
    val t = TransactionResponse(t_id, from_user_id = offer.from_user_id, to_user_id = offer.to_user_id,
      amount = offer.hours, now, offer_id = Option(offer.offer_id))

    Offers.completeOffer(offer) flowWith {_ => store(t, TransactionType.offer)}
  }

  /** HOT. The database access can fail giving wrong balances
    * @return all transaction amounts summed up */
  def getBalance(user: UserAccount): FutureErrorFlow[BigDecimal] = {
    val q = for {
      in <- DbSchema.transactions.filter(_.to === user.user_id).map(_.amount).sum.result
      out <- DbSchema.transactions.filter(_.from === user.user_id).map(_.amount).sum.result
    } yield in.getOrElse(0: BigDecimal) - out.getOrElse(0: BigDecimal)

    Db.run(q) map {Right(_)} recover {
      case e => logger.error(s"Failed during database access when getting wallet balance for user ${user.user_id}: " +
        s"${e.getMessage}")
        Left(DatabaseError("we couldn't get your balance"))
    }
  }

  private def store(t: Transaction, transaction_type: TransactionType): TransactionResult = {
    val storable = asStorable(t, transaction_type)
    Db.run(DbSchema.transactions += storable) map {_ => Right(t)} recover {
      case e => logger.error(s"Could not store transaction: ${e.getMessage}")
        Left(DatabaseError("we couldn't store this transaction"))
    }
  }

  /** @return instance that can be put into a database */
  private def asStorable(t: Transaction, ttype: TransactionType): TransactionStorable = {
    val time = Timestamp.from(Instant.ofEpochMilli(t.timestamp))
    TransactionStorable(t.transaction_id, from_user_id = t.from_user_id, to_user_id = t.to_user_id,
      amount = t.amount, timestamp = time, t.offer_id, transaction_type = ttype.toString)
  }
}
