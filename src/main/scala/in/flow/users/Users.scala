package in.flow.users

import java.security.PublicKey
import java.util.Base64
import java.util.concurrent.TimeUnit

import in.flow.commformats.InternalCommFormats.UserConnectionType
import in.flow.{WithErrorFlow, global_sha}
import in.flow.db.{Db, DbSchema, UserAccountConnectionStorable, UserAccountStorable}
import in.flow.security.Encryption
import in.flow.commformats.InternalCommFormats.UserConnectionType.UserConnectionType
import in.flow.commformats.UserAccountConnection
import in.flow.users.registration.Invitation
import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3
import sun.misc.BASE64Encoder
import scribe._

import scala.concurrent.{Await, Future}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.control.Exception.allCatch

/**
  * Deals with users, such as user lookup, creation, etc
  * todo.
  *  - maybe this object should be creating the users, not registrar
  */
object Users {
  private val logger = "Users".logger

  /** @see [[getUser(String)]]*/
  def getUser(public_key: PublicKey): Future[Option[UserAccount]] = {
    getUserId(public_key) map {id => getUser(id)} getOrElse Future(None)
  }

  /** gets user fully loaded, but the [[UserAccount]]s of connection objects are not fully loaded */
  def getUser(id: String): Future[Option[UserAccount]] = {
    lazyGetUser(loadUserFully _)(id)
  }

  /** loads the connections of the user */
  def loadUserFully(u: UserAccount): Future[UserAccount] = {
    loadUserConnections(u)
  }

  def loadUserConnections(u: UserAccount): Future[UserAccount] = if(!u.connectionsGiven) {
    populateConnections(u)
  } else Future(u)

  /** unlike regular get user loads only the id, display_name, and public key */
  def lazyGetUser(id: String): Future[Option[UserAccount]] = {
    lazyGetUser((u: UserAccount) => {
      Future.apply(u)
    })(id)
  }

  def getUserPublicKey(user: UserAccount): PublicKey = user.public_key

  /** Hashes the key to produce a (probably) unique user id */
  def getUserId(public_key: PublicKey): Option[String] = {
    allCatch[String].either({
      val id_byte = global_sha.digest(public_key.getEncoded)
      Base64.getEncoder.encodeToString(id_byte)
    }).fold(e => {
      logger.warn(s"Failed to generate id with error: ${e.getMessage}")
      None
    }, id => Option(id))
  }

  /** unlike regular get user loads only the id, display_name, and public key */
  private def lazyGetUser(additional_operations: (UserAccount) => Future[UserAccount])(id: String):
  Future[Option[UserAccount]] = {
    Db.run(DbSchema.user_accounts.filter(_.id === id).result) flatMap {optdb =>
      val f = optdb.headOption flatMap {dbu =>
        val u = userFromStorable(dbu)
        u map additional_operations
      }
      f map {_ map {Option(_)}} getOrElse Future(None)
    }
  }

  /** @return [[UserAccount]] without connections initialized */
  private def userFromStorable(stored: UserAccountStorable): Option[UserAccount] =
    Encryption.parsePublicKey(stored.public_key) map {pk =>
      UserAccount(stored.id, stored.display_name, pk)
  }

  /** @return copy of the user account with connections loaded; if unsuccessful, returns the user unchanged */
  private def populateConnections(u: UserAccount): Future[UserAccount] = {
    Db.run(DbSchema.user_account_connections.filter(dbu => (dbu.from === u.user_id) || (dbu.to === u.user_id) ).result) map
      {db_cons =>
      // futures
      val cons_f = db_cons map {c =>
        val ctype = UserConnectionType.withName(c.connection_type)
        val cuser = if (c.to_id == u.user_id) lazyGetUser(c.from_id) else lazyGetUser(c.to_id)
        cuser -> UserAccountConnection(ctype, direction_forward = c.from_id == u.user_id)
      }
      u withUnloadedConnections cons_f
    } recover {
      case e => logger.error(s"Could not populate user ${u.user_id} connections: ${e.getMessage}"); u
    }
  }

  /** extract valid connections from the futures.
    * todo. could be implemented more efficiently */
  private[users] def awaitUnloadedConnections(cons: Seq[(Future[Option[UserAccount]], UserAccountConnection)]):
  Seq[(UserAccount, UserAccountConnection)] = {
    cons map {c =>
      val u = Await.result(c._1, Duration.create(1, TimeUnit.SECONDS))
      u -> c._2
    } collect {
      case (Some(u), t) => u -> t
    }
  }
}
