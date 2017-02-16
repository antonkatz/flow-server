package in.flow.users

import java.security.PublicKey

/**
  * Deals with users, such as user lookup, creation, etc
  */
object Users {
  def getUser(public_key: PublicKey): User = ???

  /** Hashes the key to produce a (probably) unique user id */
  private[users] def getUserId(public_key: PublicKey): String = ???
}

case class User(id: String, public_key: PublicKey)
