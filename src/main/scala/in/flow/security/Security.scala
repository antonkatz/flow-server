package in.flow.security

import javax.crypto.{KeyGenerator, SecretKey}

import in.flow.users.User

import scala.concurrent.Future

/**
  * Class holding info about the security context, such as user info, and providing methods for functionality such
  * as encryption
  */
class Security(private[this] var user: User) {
  private[this] lazy val cipher_key: SecretKey = getKey

  private def getKey = SymmetricKeyCache.retrieve(user).getOrElse({
    Encryption.generateSymmetricKey
  })
}

private object SymmetricKeyCache {
  import scala.concurrent.ExecutionContext.Implicits.global

  def save(user: User, key: SecretKey) = ???

  def retrieve(user: User): Option[SecretKey] = None

  def clearOld() = Future {???}
}