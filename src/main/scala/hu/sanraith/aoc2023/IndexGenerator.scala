package hu.sanraith.aoc2023

import java.nio.file._
import scala.jdk.CollectionConverters.*
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

object IndexGenerator {
  def run() = {
    val dayFileClassNameRegex = """^(Day\d+).scala$""".r
    val root = Paths.get(System.getProperty("user.dir"))
    val sourcePath = Paths.get("src", "main")

    // Get list of solution classes
    val solutionPath = Paths.get(
      sourcePath.toString,
      "scala",
      "hu",
      "sanraith",
      "aoc2023",
      "solution"
    )
    val classNameList = Files
      .list(root.resolve(solutionPath))
      .filter(Files.isRegularFile(_))
      .toList
      .asScala
      .flatMap(x =>
        dayFileClassNameRegex
          .findFirstMatchIn(x.getFileName.toString)
          .map(_.group(1))
      )

    // Get template for Index file
    val templatePath = Paths.get("src", "templates")
    val indexFileRelativePath: Path = Paths.get(
      "scala",
      "hu",
      "sanraith",
      "aoc2023",
      "solution",
      "Index.Scala"
    )
    val indexTemplatePath = root.resolve(
      Paths.get(templatePath.toString(), indexFileRelativePath.toString())
    )
    val indexTemplate = readUtf8File(indexTemplatePath.toString())

    // Fill template with class list and save it
    val indexContents = TemplateFiller(indexTemplate)
      .fill("__SOLUTION_CLASS_LIST__", classNameList.map(x => s"classOf[$x]"))
      .toString()
    val indexFilePath = root.resolve(
      Paths.get(sourcePath.toString(), indexFileRelativePath.toString())
    )
    writeToUtf8File(indexFilePath.toString(), indexContents)
  }

  def writeToUtf8File(fileName: String, contents: String) = {
    Files.writeString(Paths.get(fileName), contents, UTF_8)
  }

  def readUtf8File(fileName: String): String = {
    Files.readString(Paths.get(fileName), UTF_8)
  }
}
