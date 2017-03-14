package in.flow.db

import in.flow.users.UserAccount
import in.flow.users.registration.{Invitation}
import slick.lifted.Shape._
//import slick.jdbc.PostgresProfile._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import slick.lifted.ShapedValue
import slick.lifted.ProvenShape

/**
  * Created by anton on 06/03/17.
  */
class UserAccounts(tag: Tag) extends Table[UserAccountStorable](tag, "user_accounts") {
  def id = column[String]("id", O.PrimaryKey)
  def displayName = column[String]("display_name")
  def publicKey = column[Array[Byte]]("public_key")

  def * : ProvenShape[UserAccountStorable] = (id, displayName, publicKey) <> (UserAccountStorable.tupled,
    UserAccountStorable.unapply)
}

class Invitations(tag: Tag) extends Table[InvitationStorable](tag, "invitations") {
  def code = column[String]("code", O.PrimaryKey)
  def userId = column[String]("user")

  def user = foreignKey("user_fk", userId, DbSchema.user_accounts)(_.id)

  override def * : ProvenShape[InvitationStorable] = (userId, code) <> (InvitationStorable.tupled, InvitationStorable.unapply)
}

class UserAccountConnections(tag: Tag) extends Table[UserAccountConnectionStorable](tag, "user_account_connections") {
  def from = column[String]("from")
  def to = column[String]("to")
  def conType = column[String]("connection_type")

  def userFrom = foreignKey("user_fk_from", from, DbSchema.user_accounts)(_.id)
  def userTo = foreignKey("user_fk_to", from, DbSchema.user_accounts)(_.id)

  def * = (from, to, conType) <> (UserAccountConnectionStorable.tupled, UserAccountConnectionStorable.unapply)
}


object DbSchema {
  val user_account_connections: TableQuery[UserAccountConnections] = TableQuery[UserAccountConnections]
  val user_accounts: TableQuery[UserAccounts] = TableQuery[UserAccounts]
  val invitations: TableQuery[Invitations] = TableQuery[Invitations]
}

case class UserAccountStorable(id: String, display_name: String, public_key: Array[Byte])

case class InvitationStorable(user_id: String, code: String)

case class UserAccountConnectionStorable(from_id: String, to_id: String, connection_type: String)