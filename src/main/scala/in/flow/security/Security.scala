package in.flow.security

import java.security.PublicKey
import javax.crypto.SecretKey

import in.flow.users.{UserAccount, Users}
import org.bouncycastle.util.encoders.Hex
import scribe.Logger

import scala.concurrent.Future
import scala.util.control.Exception._

/**
  * Class holding info about the security context, such as user info, and providing methods for functionality such
  * as encryption.
  * It takes on the responsibility of converting different formats such as base64 and hex and plain bytes, to suit the
  * needs of different libraries on the clients side
  */
class Security(private var user: Option[UserAccount], private val decryption_iv: Option[Array[Byte]]) {
  // todo. the null is pretty bad, but if it comes out null, there better be no use for it (execute function should
  // make sure of that)
  private[this] var symmetric_key: Either[SecretKey, SecretKey] = null

  private def initSymmetricKey = symmetric_key = Security.getOrGenerateKey(this).getOrElse(null)

  private def getSymmetricKey: SecretKey = symmetric_key.fold(k => k, k => k)

  /** indicates if the cipher key came from cache or was generated; if generated, user does not have a copy */
  private def isSymmetricKeyNew = symmetric_key.isLeft

  /** for cases where information has to be sent back, but the sender is not a registered user */
  private var senders_public_key: Option[PublicKey] = None

  private var error: Option[Throwable] = None

  private var canExecute = true

  initSymmetricKey
}
object Security {
  import in.flow.getLogger
  private val logger: Logger = "SecurityObject"
  /**
    * Creates a security context with user, initialization vector.
    * */
  def apply(user: Option[UserAccount], iv: Option[Array[Byte]]): Security = {
    new Security(user, iv)
  }

  /** for making sure that anything does not get executed if execution is not allowed. Executes the given function and
    * catches any exception. If an exception occurs, security context is marked as non executable */
  // todo. make it => T
  def execute[T](f: () => T)(implicit s: Security): Either[Throwable, T] = {
    if (s.canExecute)
      catchExceptionToSecurity(f)
    else {
      Left(s.error getOrElse new UnknownError())
    }
  }

  private def catchExceptionToSecurity[T](function: () => T)(implicit s: Security): Either[Throwable, T] = {
    val res = allCatch either function()
    s.error = res.left.toOption
    if (s.error.nonEmpty) {
      logger.error(s"Security module caught exception: ${s.error.get.getMessage}")
      s.canExecute = false
    }
    res
  }

  /** decrypts the payload in the context of security (asymmetric vs symmetric), and in case of errors notifies the
    * security instance */
  def receivePayload(payload: Array[Byte])(implicit s: Security): Either[Throwable, Array[Byte]] = {
    execute[Array[Byte]](() => receivePayload_(payload))
  }

  private def receivePayload_(payload: Array[Byte])(implicit s: Security): Array[Byte] = s.decryption_iv match {
    case Some(iv) => if (!s.isSymmetricKeyNew) {
      Encryption.receive(payload, s.getSymmetricKey, iv)
    } else {
      throw new NeedSymmetricKey
    }
    case None => Encryption.receiveAsBytes(payload)
  }

  def sendPayload(payload: String)(implicit s: Security): Either[Throwable, SymmetricallyEncrypted] = execute {() =>
    val res = Encryption.send(payload, s.getSymmetricKey)
    res
  }

  /** if the user needs a copy of the cipher, then this will return it, encrypted with the public key */
  def sendSymmetricKey(implicit s: Security): Option[Array[Byte]] = {
    execute({() => // add if new only
      if(s.isSymmetricKeyNew) {
        val skey = Hex.toHexString(s.getSymmetricKey.getEncoded)
        getPublicKey match {
          case Some(pk) =>
            s.user foreach {u => SymmetricKeyCache.save(u, s.getSymmetricKey)}
            Some(Encryption.send(skey, pk))
          case _ => throw new NeedAsymmetricKey
        }
      } else None
    }).right.toOption.flatten
  }

  def provideError(implicit s: Security): Option[Throwable] = s.error

  /** uses a pem parser (requires the BEGIN/END directive) */
  def setPublicKey(key_string: String)(implicit s: Security): Option[Throwable] = {
    execute[Unit] {() =>
      s.senders_public_key = Encryption.parsePublicKey(key_string)
      if (s.senders_public_key.isEmpty) throw new NeedAsymmetricKey
    }.left.toOption
  }

  /** @return either the stored public key of an existing user, or the temporarily held public key */
  def getPublicKey(implicit s: Security): Option[PublicKey] = {
    s.senders_public_key match {
      case None => s.user map Users.getUserPublicKey
      case o => o
    }
  }

  def getUserId(implicit security: Security): Option[String] = security.user.map(_.user_id)

  def setUser(user: UserAccount)(implicit s: Security): Unit = s.user = Option(user)

  def refreshSymmetricKey(implicit s: Security) = {
    s.user foreach SymmetricKeyCache.delete
    s.initSymmetricKey
  }

  /** @return [[Left]] is new, [[Right]] if retrieved from cache */
  private def getOrGenerateKey(implicit security: Security) = execute[Either[SecretKey, SecretKey]] {() =>
    (security.user flatMap SymmetricKeyCache.retrieve).fold[Either[SecretKey, SecretKey]] ({
    val k = Encryption.generateSymmetricKey
    Left(k)
  })(Right(_))
  }
}

/** keeps a certain number of keys in memory; overwrites them round-robin style */
private object SymmetricKeyCache {
  private val cache_size = 1000
  private val cache_user = new Array[String](cache_size)
  private val cache_key = new Array[SecretKey](cache_size)
  private var cursor = -1  // starting value

  def getCursor: Int = {
    cursor += 1
    if (cursor >= cache_size) cursor = 0
    cursor
  }

  def delete(user: UserAccount) = {
    cache_user indexOf user.user_id match {
      case -1 => Unit
      case index =>
        cache_user(index) = null
        cache_key(index) = null
    }
  }

  def save(user: UserAccount, key: SecretKey) = {
    val c = getCursor
    cache_user(c) = user.user_id
    cache_key(c) = key
  }

  def retrieve(user: UserAccount): Option[SecretKey] = {
    cache_user indexOf user.user_id match {
      case -1 => None
      case index => Option(cache_key(index))
    }
  }
}

class NeedSymmetricKey extends Error

class NeedAsymmetricKey extends Error