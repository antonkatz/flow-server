package in.flow.users

import java.security.PublicKey

import in.flow.commformats.UserAccountConnection
import in.flow.db.UserAccountStorable

import scala.concurrent.Future
import in.flow.getLogger
import scribe.Logger
// todo. the logic must be taken out of this class
/**
  * Created by anton on 23/03/17.
  *
  */
case class UserAccount(user_id: String, display_name: String, public_key: PublicKey) {
  private val logger: Logger = "UserAccount"
  /** has the connections been loaded, or was this user lazy loaded? */
  private var loaded_connections_flag = false
  private var unloaded_connections_flag = false

  private var unloaded_connections: Seq[(Future[Option[UserAccount]], UserAccountConnection)] = Nil

  private var _connections:Seq[(UserAccount, UserAccountConnection)] = Nil

  private[users] def withUnloadedConnections(cons:
                                            Seq[(Future[Option[UserAccount]], UserAccountConnection)]): UserAccount = {
    unloaded_connections = cons
    unloaded_connections_flag = true
    this
  }

  def connections: Seq[(UserAccount, UserAccountConnection)] = if (loaded_connections_flag) {
    _connections
  } else {
    _connections = Users.awaitUnloadedConnections(unloaded_connections)
    if (_connections.isEmpty) logger.error(s"There should be at least one connection for user $user_id")
    loaded_connections_flag = true
    _connections
  }

  /** @return true if this user has been given connections; they might not be necessarily loaded. */
  def connectionsGiven = unloaded_connections_flag

  implicit def storable: UserAccountStorable = UserAccountStorable(user_id, display_name, public_key.getEncoded)
}
