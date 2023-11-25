package hu.sanraith.aoc2023.cli

import pureconfig._
import java.nio.file.{Files, Paths}
import scala.util.Using
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigRenderOptions

case class AppConfig(
    sessionCookie: Option[String],
    pathToEditor: Option[String],
    openScaffoldedFiles: Boolean,
    copyResultToClipboard: Boolean
)

object AppConfig:
  val ConfigFileName = "config.json"
  lazy val instance = loadFromFile(ConfigFileName)

  val defaultConfig: AppConfig =
    AppConfig(
      sessionCookie = None,
      pathToEditor = None,
      openScaffoldedFiles = false,
      copyResultToClipboard = false
    )

  def saveToFile(preferences: AppConfig, filePath: String): Unit = {
    val config = ConfigWriter[AppConfig].to(preferences)
    Files.write(
      Paths.get(filePath),
      config.render(ConfigRenderOptions.defaults().setOriginComments(false)).getBytes
    )
  }

  def loadFromFile(filePath: String): AppConfig = {
    if (Files.exists(Paths.get(filePath))) {
      val content = Using.resource(scala.io.Source.fromFile(filePath)) { source =>
        source.mkString
      }

      val rawConfig = ConfigFactory.parseString(content)
      ConfigSource.fromConfig(rawConfig).load[AppConfig].getOrElse(defaultConfig)
    } else {
      saveToFile(defaultConfig, filePath)
      defaultConfig
    }
  }

  implicit val configReader: ConfigReader[AppConfig] =
    ConfigReader.forProduct4(
      "sessionCookie",
      "pathToEditor",
      "openScaffoldedFiles",
      "copyResultToClipboard"
    )((a: String, b: String, c: Boolean, d: Boolean) => {
      val toOpt = (x: String) => Option(x).filter(_.trim.nonEmpty)
      AppConfig(toOpt(a), toOpt(b), c, d)
    })

  implicit val configWriter: ConfigWriter[AppConfig] =
    ConfigWriter.forProduct4(
      "sessionCookie",
      "pathToEditor",
      "openScaffoldedFiles",
      "copyResultToClipboard"
    )(x =>
      (
        x.sessionCookie.getOrElse(""),
        x.pathToEditor.getOrElse(""),
        x.openScaffoldedFiles,
        x.copyResultToClipboard
      )
    )
