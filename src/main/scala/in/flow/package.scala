package in

import java.time.{ZoneId, ZonedDateTime}

import org.bouncycastle.jcajce.provider.digest.SHA3.DigestSHA3
import scribe.{Level, LogHandler, Logger}
import scribe.formatter.FormatterBuilder

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Global variables
  */
package object flow {
  /* variables and functions for global consistency */

  val global_string_format = "UTF-8"
  val global_sha = new DigestSHA3(256)
  def getNow = {
    ZonedDateTime.now(ZoneId.of("UTC")).toInstant
  }

  /* logger */

  val lf = FormatterBuilder()
    .levelPaddedRight.string(" ").date()
    .string(" [").className.string("] ")
    .positionAbbreviated.newLine
    .message.newLine
  Logger.root.clearHandlers()
  Logger.root.addHandler(LogHandler(formatter = lf, level = Level.Debug))

  implicit val getLogger: (String) => Logger = { (name: String) => Logger.byName(name)}

  /* error flow */

  type WithErrorFlow[T] = Either[FlowError, T]
  type FutureErrorFlow[T] = Future[WithErrorFlow[T]]

  implicit class FlowableFuture[T](f: Future[WithErrorFlow[T]]) {
    /** Transforms this future's (if it is successful) internal [[WithErrorFlow]] right projection with the given
      * function; propagates [[Left]] with no changes. */
    def flowWith[O](next: (T) => FutureErrorFlow[O]): FutureErrorFlow[O] = f flatMap {
      either => either.fold(e => Future(Left(e)), s => next(s))
    }

    /** Transforms this future's (if it is successful) internal [[WithErrorFlow]] with given function (if the
      * [[WithErrorFlow]] is [[Right]]). Propagates the [[Left]] otherwise. */
    def flowRight[O](next: (T) => O): FutureErrorFlow[O] = f map {
      either => either.fold(e => Left(e), s => Right(next(s)))
    }
  }

  /* errors */

  trait FlowError extends scala.Error {
    def isUser: Boolean = !isServer
    def isServer: Boolean = !isUser
    def errorCode: String
    val message: String

    override def toString = s"$errorCode - $message - fault of ${if (isUser) "user" else "server"}"
  }

  trait ServerError extends FlowError {
    override def isServer = true
  }

  trait UserError extends FlowError {
    override def isUser = true
  }

  case class InvalidInputError(message: String) extends UserError {
    override def errorCode = "invalid_input_error"
  }
  case class MissingPublicKeyError() extends UserError {
    override def errorCode: String = "missing_public_key_error"

    override val message: String = "we can't find your public key"
  }
  /* todo. Unify security and Flow errors */
  case class MissingUserError() extends UserError {
    override def errorCode: String = "missing_user"

    override val message: String = "we don't know who is the user trying this action"
  }
  case class DatabaseError(message: String) extends ServerError {
    override def errorCode = "database_error"
  }
  case class UnknownError() extends ServerError {
    override def errorCode: String = "unknown_error"

    override val message: String = "we don't know what happened"
  }
}
