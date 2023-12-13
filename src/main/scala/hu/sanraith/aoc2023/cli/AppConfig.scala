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
    copyResultToClipboard: Boolean,
    eventYear: Int
)

object AppConfig:
  val ConfigFileName = "config.json"
  lazy val instance = loadFromFile(ConfigFileName)

  val defaultConfig: AppConfig =
    AppConfig(
      sessionCookie = Some(""), // TODO fix these so that empty string is read as None
      pathToEditor = Some(""), // TODO fix these so that empty string is read as None
      openScaffoldedFiles = false,
      copyResultToClipboard = false,
      eventYear = 2023
    )

  def saveToFile(preferences: AppConfig, filePath: String): Unit =
    val config = ConfigWriter[AppConfig].to(preferences)
    Files.write(
      Paths.get(filePath),
      config.render(ConfigRenderOptions.defaults().setOriginComments(false)).getBytes
    )

  def loadFromFile(filePath: String): AppConfig =
    if (Files.exists(Paths.get(filePath)))
      val content = Using.resource(scala.io.Source.fromFile(filePath))(source => source.mkString)
      val rawConfig = ConfigFactory.parseString(content)
      ConfigSource.fromConfig(rawConfig).load[AppConfig].getOrElse(defaultConfig)
    else
      saveToFile(defaultConfig, filePath)
      defaultConfig

  implicit val configReader: ConfigReader[AppConfig] = ConfigReader.forProduct5(
    "sessionCookie",
    "pathToEditor",
    "openScaffoldedFiles",
    "copyResultToClipboard",
    "eventYear"
  )(AppConfig.apply)

  implicit val configWriter: ConfigWriter[AppConfig] = ConfigWriter.forProduct5(
    "sessionCookie",
    "pathToEditor",
    "openScaffoldedFiles",
    "copyResultToClipboard",
    "eventYear"
  )(c =>
    (c.sessionCookie, c.pathToEditor, c.openScaffoldedFiles, c.copyResultToClipboard, c.eventYear)
  )
