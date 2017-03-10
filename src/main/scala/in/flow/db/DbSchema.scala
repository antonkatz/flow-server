package in.flow.db

import in.flow.users.UserAccount
import in.flow.users.registration.{Invitation, InvitationStored}
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
class UserAccounts(tag: Tag) extends Table[UserAccount](tag, "user_accounts") {
  def id = column[String]("id", O.PrimaryKey)
  def displayName = column[String]("display_name")

  def * : ProvenShape[UserAccount] = (id, displayName) <> (UserAccount.tupled, UserAccount.unapply)
}

class Invitations(tag: Tag) extends Table[InvitationStored](tag, "invitations") {
  def code = column[String]("code", O.PrimaryKey)
  def userId = column[String]("user")

  def user = foreignKey("user_fk", userId, DbSchema.user_accounts)(_.id)

  override def * : ProvenShape[InvitationStored] = (userId, code) <> (InvitationStored.tupled, InvitationStored.unapply)
}

object DbSchema {
  val user_accounts: TableQuery[UserAccounts] = TableQuery[UserAccounts]
  val invitations: TableQuery[Invitations] = TableQuery[Invitations]
}