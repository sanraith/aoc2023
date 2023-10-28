package hu.sanraith.aoc2023

import java.nio.file._
import scala.jdk.CollectionConverters.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

object FileGenerator {
  private val dayFileClassNameRegex = """^(Day\d+).scala$""".r
  private val root = Paths.get(System.getProperty("user.dir"))

  private val sourcePart = Paths.get("src", "main")
  private val namespacePart =
    Paths.get("scala", "hu", "sanraith", "aoc2023")

  private val templateRoot = Paths.get("src", "templates")
  private val solutionRoot = Paths.get(
    sourcePart.toString,
    namespacePart.toString,
    "solution"
  )

  def generateSolutionFile(day: Int, title: String) = {
    val dayStr =
      if (day < 10) s"0$day"
      else day.toString
    val templatePath =
      root.resolve(
        Paths.get(
          templateRoot.toString,
          namespacePart.toString,
          "solution",
          "Day__DAY_STR__.scala"
        )
      )
    val template = readUtf8File(templatePath.toString)
    val contents = TemplateFiller(template)
      .fill("__DAY_STR__", dayStr)
      .fill("__TITLE__", title)
      .toString
    val solutionPath =
      root.resolve(Paths.get(solutionRoot.toString, s"Day$dayStr.scala"))
    writeToUtf8File(solutionPath.toString, contents)
  }

  def generateIndexFile() = {
    val classNameList = Files
      .list(root.resolve(solutionRoot))
      .filter(Files.isRegularFile(_))
      .toList
      .asScala
      .flatMap(x =>
        dayFileClassNameRegex
          .findFirstMatchIn(x.getFileName.toString)
          .map(_.group(1))
      )

    // Get template for Index file
    val indexFilePath: Path = Paths.get(
      namespacePart.toString,
      "solution",
      "Index.Scala"
    )
    val templatePath = root.resolve(
      Paths.get(templateRoot.toString(), indexFilePath.toString())
    )
    val template = readUtf8File(templatePath.toString())

    // Fill template with class list and save it
    val contents = TemplateFiller(template)
      .fill("__SOLUTION_CLASS_LIST__", classNameList.map(x => s"classOf[$x]"))
      .toString()
    val resultPath = root.resolve(
      Paths.get(sourcePart.toString(), indexFilePath.toString())
    )
    writeToUtf8File(resultPath.toString(), contents)
  }

  def writeToUtf8File(fileName: String, contents: String) = {
    Files.writeString(Paths.get(fileName), contents, UTF_8)
  }

  def readUtf8File(fileName: String): String = {
    Files.readString(Paths.get(fileName), UTF_8)
  }
}
