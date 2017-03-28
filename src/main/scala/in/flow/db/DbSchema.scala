package in.flow.db

import java.time.LocalDateTime

import in.flow.db.OfferStatusType.OfferStatusType
import in.flow.users.UserAccount
import in.flow.users.registration.Invitation
import slick.lifted.Shape._
//import slick.jdbc.PostgresProfile._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import slick.lifted.ShapedValue
import slick.lifted.ProvenShape
import java.sql.Timestamp

/**
  * Created by anton on 06/03/17.
  */
class UserAccountsTable(tag: Tag) extends Table[UserAccountStorable](tag, "user_accounts") {
  def id = column[String]("id", O.PrimaryKey)
  def displayName = column[String]("display_name")
  def publicKey = column[Array[Byte]]("public_key")

  def * : ProvenShape[UserAccountStorable] = (id, displayName, publicKey) <> (UserAccountStorable.tupled,
    UserAccountStorable.unapply)
}

class InvitationsTable(tag: Tag) extends Table[InvitationStorable](tag, "invitations") {
  def code = column[String]("code", O.PrimaryKey)
  def userId = column[String]("user")

  def user = foreignKey("user_fk", userId, DbSchema.user_accounts)(_.id)

  override def * : ProvenShape[InvitationStorable] = (userId, code) <> (InvitationStorable.tupled, InvitationStorable.unapply)
}

class UserAccountConnectionsTable(tag: Tag) extends
  Table[UserAccountConnectionStorable](tag, "user_account_connections") {
  def id = column[String]("connection_id", O.PrimaryKey) // hash of from and to
  def from = column[String]("from")
  def to = column[String]("to")
  def conType = column[String]("connection_type")

  def userFrom = foreignKey("user_fk_from", from, DbSchema.user_accounts)(_.id)
  def userTo = foreignKey("user_fk_to", to, DbSchema.user_accounts)(_.id)

  def * = (id, from, to, conType) <> (UserAccountConnectionStorable.tupled, UserAccountConnectionStorable.unapply)
}

class OffersTable(tag: Tag) extends Table[OfferStorable](tag, "offers") {
  def offerId = column[String]("offer_id", O.PrimaryKey)
  def from = column[String]("from_id")
  def to = column[String]("to_id")
  def hours = column[BigDecimal]("hours")
  def description = column[String]("description")
  def timestamp_created = column[Timestamp]("timestamp_created")
  /* updates */
  def status = column[String]("status")
  def timestamp_updated = column[Timestamp]("timestamp_updated")

  def userFrom = foreignKey("user_fk_from", from, DbSchema.user_accounts)(_.id)
  def userTo = foreignKey("user_fk_to", to, DbSchema.user_accounts)(_.id)

  def * = (offerId, from, to, hours, description, timestamp_created,
  status, timestamp_updated) <> (OfferStorable.fromDb, OfferStorable.toDb)
}

class TransactionsTable(tag: Tag) extends Table[TransactionStorable](tag, "transactions") {
  def transactionId = column[String]("transaction_id", O.PrimaryKey)
  def parentId = column[Option[String]]("parent_id")
  def from = column[String]("from_id")
  def to = column[String]("to_id")
  def amount = column[BigDecimal]("amount")
  def timestamp = column[Timestamp]("timestamp_created")
  def offerId = column[Option[String]]("offer_id")
  def transaction_type = column[String]("type")

  def userFrom = foreignKey("user_fk_from", from, DbSchema.user_accounts)(_.id)
  def userTo = foreignKey("user_fk_to", to, DbSchema.user_accounts)(_.id)
  def offerIdFk = foreignKey("offer_fk", offerId, DbSchema.offers)(_.offerId)
  def parentIdFk = foreignKey("parent_fk", parentId, DbSchema.transactions)(_.transactionId)

  def * = (transactionId, parentId, from, to, amount, timestamp,
    offerId, transaction_type) <> (TransactionStorable.tupled, TransactionStorable.unapply)
}

object DbSchema {
  val user_account_connections: TableQuery[UserAccountConnectionsTable] = TableQuery[UserAccountConnectionsTable]
  val user_accounts: TableQuery[UserAccountsTable] = TableQuery[UserAccountsTable]
  val invitations: TableQuery[InvitationsTable] = TableQuery[InvitationsTable]
  val offers: TableQuery[OffersTable] = TableQuery[OffersTable]
  val transactions: TableQuery[TransactionsTable] = TableQuery[TransactionsTable]
}

case class UserAccountStorable(id: String, display_name: String, public_key: Array[Byte])

case class InvitationStorable(user_id: String, code: String)

case class UserAccountConnectionStorable(connection_id: String, from_id: String, to_id: String, connection_type: String)

/* offers */

case class OfferStorable(offer_id: String, from_user_id: String, to_user_id: String, hours: BigDecimal, description:
String, timestamp_created: Timestamp, status: OfferStatusType, timestamp_updated: Timestamp)

object OfferStorable {
  def fromDb(t: Tuple8[String, String, String, BigDecimal, String, Timestamp, String, Timestamp]): OfferStorable = {
    val update_time = t._8
    val status = OfferStatusType.withName(t._7)
    OfferStorable(t._1, t._2, t._3, t._4, t._5, t._6, status, update_time)
  }
  def toDb(o: OfferStorable): Option[Tuple8[String, String, String, BigDecimal, String, Timestamp, String, Timestamp]] = {
    Option((o.offer_id, o.from_user_id, o.to_user_id, o.hours, o.description, o.timestamp_created,
      o.status.toString, o.timestamp_updated))
  }
}

object OfferStatusType extends Enumeration {
  type OfferStatusType = Value
  val open, completed, rejected = Value
}

/* transactions / wallet */

/** @param offer_id could be empty or null */
case class TransactionStorable(transaction_id: String, parent_id: Option[String], from_user_id: String,
                               to_user_id:String, amount: BigDecimal, timestamp: Timestamp,
                               offer_id: Option[String], transaction_type: String)