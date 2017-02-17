package in.flow.security

import java.security.PublicKey
import javax.crypto.SecretKey

import in.flow.users.User

import scala.concurrent.Future
import scala.util.control.Exception._

/**
  * Class holding info about the security context, such as user info, and providing methods for functionality such
  * as encryption.
  * It takes on the responsibility of converting different formats such as base64 and hex and plain bytes, to suit the
  * needs of different libraries on the clients side
  */
class Security(private val user: Option[User], private val decryption_iv: Option[Array[Byte]]) {
  private val cipher_key: SecretKey = Security.getKey(this)

  private val return_public_key: Option[PublicKey] = None

  private var error: Option[Throwable] = None

  private var canExecute = true
}

object Security {
  def apply(user: Option[User], iv: Option[Array[Byte]]): Security = {
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
    s.canExecute = false
    res
  }

  /** decrypts the payload in the context of security (asymmetric vs symmetric), and in case of errors notifies the
    * security instance */
  def receivePayload(payload: Array[Byte])(implicit s: Security): Either[Throwable, Array[Byte]] = {
    execute[Array[Byte]](() => receivePayload_(payload))
  }

  private def receivePayload_(payload: Array[Byte])(implicit s: Security): Array[Byte] = s.decryption_iv match {
    case Some(iv) => Encryption.receive(payload, s.cipher_key, iv)
    case None => Encryption.receiveAsBytes(payload)
  }

  /** Whether this [[Security]] can encrypt symmetrically */
  def canEncryptSymmetrically(implicit s: Security): Boolean = s.decryption_iv.fold(false)(_ => true)

  def provideError(implicit s: Security): Option[Throwable] = s.error

  /** the key never leaves this class unencrypted */
  def provideSymmetricKey(implicit s: Security): Option[Array[Byte]] = s.return_public_key map { pk =>
    Encryption.send(s.cipher_key.getEncoded, pk)
  }

  private def getKey(implicit security: Security) = security.user flatMap SymmetricKeyCache.retrieve getOrElse {
    Encryption.generateSymmetricKey
  }
}

private object SymmetricKeyCache {

  import scala.concurrent.ExecutionContext.Implicits.global

  def save(user: User, key: SecretKey) = ???

  def retrieve(user: User): Option[SecretKey] = None

  def clearOld() = Future {
    ???
  }
}