package in.flow.users

import java.security.PublicKey

import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3
import sun.misc.BASE64Encoder
/**
  * Deals with users, such as user lookup, creation, etc
  */
object Users {
  private val sha = new DigestSHA3(256)
  private val base64_encoder = new BASE64Encoder()

  def getUser(public_key: PublicKey): Option[UserAccount] = {
    val key_bytes = public_key.getEncoded
    val hash = sha.digest(key_bytes)
    val base64 = base64_encoder.encode(hash)
    getUser(base64)
  }

  def getUser(id: String): Option[UserAccount] = ???

  def getUserPublicKey(user: BasicUserAccount): Option[PublicKey] = ???

  /** Hashes the key to produce a (probably) unique user id */
  private[users] def getUserId(public_key: PublicKey): String = ???
}

trait BasicUserAccount {
  val id: String
}

case class UserAccount(id: String) extends BasicUserAccount