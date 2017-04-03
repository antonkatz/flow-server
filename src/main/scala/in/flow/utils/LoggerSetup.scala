package in.flow.utils

import java.io.File

import scribe.formatter.FormatterBuilder
import scribe.writer.FileWriter
import scribe.{Level, LogHandler, Logger}

/**
  * For configuring logger based on production/no production
  */
object LoggerSetup {
  private val file = scala.io.Source.fromFile("settings")
  private val settings = file.getLines().toSeq
  private val is_production = settings.lift(4).exists(_ == "true")
  private val info_log_dir = new File(settings.lift(6).getOrElse("/tmp"))
  private val debug_log_dir = new File(settings.lift(7).getOrElse("/tmp"))


  val lf = FormatterBuilder()
    .levelPaddedRight.string(" ").date()
    .string(" [").className.string("] ")
    .positionAbbreviated.newLine
    .message.newLine

  Logger.root.clearHandlers()

  if (is_production) {
    Logger.root.addHandler(LogHandler(formatter = lf, level = Level.Info,
      writer = FileWriter.daily("info", info_log_dir)))
    Logger.root.addHandler(LogHandler(formatter = lf, level = Level.Debug,
        writer = FileWriter.daily("debug", debug_log_dir)))
  } else {
    Logger.root.addHandler(LogHandler(formatter = lf, level = Level.Debug))
  }

  implicit def getLogger(name: String): Logger = {
    Logger.byName(name)
  }
}
