package in

import scribe.{Level, LogHandler, Logger}
import scribe.formatter.FormatterBuilder

/**
  * Global variables
  */
package object flow {
  val global_string_format = "UTF-8"

  val lf = FormatterBuilder()
    .levelPaddedRight.string(" ").date()
    .string(" [").className.string("] ")
    .positionAbbreviated.newLine
    .message.newLine
  Logger.root.clearHandlers()
  Logger.root.addHandler(LogHandler(formatter = lf, level = Level.Debug))

  implicit val getLogger: (String) => Logger = { (name: String) => Logger.byName(name)}

  type FlowResponseType[T] = Either[FlowError, T]

  trait FlowError extends scala.Error {
    def isUser: Boolean = !isServer
    def isServer: Boolean = !isUser
    def errorCode: String
    val message: String
  }

  trait ServerError extends FlowError {
    override def isServer = true
  }

  trait UserError extends FlowError {
    override def isUser = true
  }

  case class DatabaseError(message: String) extends ServerError {
    override def errorCode = "database_error"
  }
  case class InvalidInputError(message: String) extends UserError {
    override def errorCode = "invalid_input_error"
  }
  case class MissingPublicKeyError() extends UserError {
    override def errorCode: String = "missing_public_key_error"

    override val message: String = "we can't find your public key"
  }
  case class UnknownError() extends ServerError {
    override def errorCode: String = "unknown_error"

    override val message: String = "we don't know what happened"
  }
}
