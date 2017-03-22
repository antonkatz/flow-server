package in.flow.db

import java.time.LocalDateTime

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
  def hours = column[Float]("hours")
  def description = column[String]("description")
  def timestamp = column[Timestamp]("timestamp")

  def userFrom = foreignKey("user_fk_from", from, DbSchema.user_accounts)(_.id)
  def userTo = foreignKey("user_fk_to", to, DbSchema.user_accounts)(_.id)

  def * = (offerId, from, to, hours, description, timestamp) <> (OfferStorable.tupled, OfferStorable.unapply)
}

object DbSchema {
  val user_account_connections: TableQuery[UserAccountConnectionsTable] = TableQuery[UserAccountConnectionsTable]
  val user_accounts: TableQuery[UserAccountsTable] = TableQuery[UserAccountsTable]
  val invitations: TableQuery[InvitationsTable] = TableQuery[InvitationsTable]
  val offers: TableQuery[OffersTable] = TableQuery[OffersTable]
}

case class UserAccountStorable(id: String, display_name: String, public_key: Array[Byte])

case class InvitationStorable(user_id: String, code: String)

case class UserAccountConnectionStorable(connection_id: String, from_id: String, to_id: String, connection_type: String)

case class OfferStorable(offer_id: String, from_user_id: String, to_user_id: String, hours: Float, description:
String, timestamp: Timestamp = Timestamp.valueOf(LocalDateTime.now()))