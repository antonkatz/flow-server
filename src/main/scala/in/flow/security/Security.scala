package in.flow.security

import java.security.PublicKey
import javax.crypto.SecretKey

import in.flow.users.{UserAccount, Users}
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.Future
import scala.util.control.Exception._

/**
  * Class holding info about the security context, such as user info, and providing methods for functionality such
  * as encryption.
  * It takes on the responsibility of converting different formats such as base64 and hex and plain bytes, to suit the
  * needs of different libraries on the clients side
  */
class Security(private var user: Option[UserAccount], private val decryption_iv: Option[Array[Byte]]) {
  private lazy val symmetric_key: SecretKey = Security.getOrGenerateKey(this)

  /** indicates if the cipher key came from cache or was generated; if generated, user does not have a copy */
  private var symmetric_key_is_new = true

  /** for cases where information has to be sent back, but the sender is not a registered user */
  private var senders_public_key: Option[PublicKey] = None

  private var error: Option[Throwable] = None

  private var canExecute = true
}

object Security {
  /**
    * Creates a security context with user, initialization vector.
    * */
  def apply(user: Option[UserAccount], iv: Option[Array[Byte]]): Security = {
    new Security(user, iv)
  }

  /** for making sure that anything does not get executed if execution is not allowed. Executes the given function and
    * catches any exception. If an exception occurs, security context is marked as non executable */
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
    if (s.error.nonEmpty) {s.canExecute = false}
    res
  }

  /** decrypts the payload in the context of security (asymmetric vs symmetric), and in case of errors notifies the
    * security instance */
  def receivePayload(payload: Array[Byte])(implicit s: Security): Either[Throwable, Array[Byte]] = {
    execute[Array[Byte]](() => receivePayload_(payload))
  }

  private def receivePayload_(payload: Array[Byte])(implicit s: Security): Array[Byte] = s.decryption_iv match {
    case Some(iv) => if (!s.symmetric_key_is_new) {
      Encryption.receive(payload, s.symmetric_key, iv)
    } else {
      throw new NeedSymmetricKey
    }
    case None => Encryption.receiveAsBytes(payload)
  }

  def sendPayload(payload: String)(implicit s: Security): Either[Throwable, SymmetricallyEncrypted] = execute {() =>
    val res = Encryption.send(payload, s.symmetric_key)
    res
  }

  /** if the user needs a copy of the cipher, then this will return it, encrypted with the public key */
  def sendSymmetricKey(implicit s: Security): Option[Array[Byte]] = {
    execute({() => // add if new only
      if(s.symmetric_key_is_new) {
        val skey = Hex.toHexString(s.symmetric_key.getEncoded)
        getPublicKey match {
          case Some(pk) => Some(Encryption.send(skey, pk))
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
      case None => s.user flatMap Users.getUserPublicKey
      case o => o
    }
  }

  /** Whether this [[Security]] can encrypt symmetrically
    * DO NOT USE, FLAWED*/
//  private def canEncryptSymmetrically(implicit s: Security): Boolean = s.decryption_iv.fold(false)(_ => true)

  private def getOrGenerateKey(implicit security: Security) = (security.user flatMap SymmetricKeyCache.retrieve).fold {
    security.symmetric_key_is_new = true
    Encryption.generateSymmetricKey
  } {k => security.symmetric_key_is_new = false; k}
}

private object SymmetricKeyCache {

  import scala.concurrent.ExecutionContext.Implicits.global

  def save(user: UserAccount, key: SecretKey) = ???

  def retrieve(user: UserAccount): Option[SecretKey] = None

  def clearOld() = Future {
    ???
  }
}

class NeedSymmetricKey extends Error

class NeedAsymmetricKey extends Error