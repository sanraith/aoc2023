package hu.sanraith.aoc2023.cli

import java.nio.file._
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object FileManager:
  private val DayFileClassNameRegex = """^(Day\d+).scala$""".r
  private val Root = Paths.get(System.getProperty("user.dir"))

  private val SourcePart = Paths.get("src", "main")
  private val NamespacePart =
    Paths.get("scala", "hu", "sanraith", "aoc2023")

  private val TemplateRoot = Paths.get("src", "templates")
  private val SolutionRoot = Paths.get(
    SourcePart.toString,
    NamespacePart.toString,
    "solution"
  )
  private val TestRoot =
    Paths.get("src", "test", NamespacePart.toString, "solution")

  def createInputFile(day: Int, contents: String): Path =
    val dayStr = getDayStr(day)
    val path = Paths.get(s"input/Day$dayStr.txt")
    writeToUtf8File(path, contents)

  def createSolutionFile(day: Int, title: String): Path =
    val dayStr = getDayStr(day)
    val templatePath =
      Root.resolve(
        Paths.get(
          TemplateRoot.toString,
          "Day__DAY_STR__.scala"
        )
      )
    val template = readUtf8File(templatePath)
    val contents = TemplateFiller(template)
      .fill("__DAY_STR__", dayStr)
      .fill("__TITLE__", title)
      .toString
    val solutionPath =
      Root.resolve(Paths.get(SolutionRoot.toString, s"Day$dayStr.scala"))
    writeToUtf8File(solutionPath, contents)

  def createTestFile(
      day: Int,
      part1TestInput: String = "__PART_1_TEST_INPUT__",
      part1TestExpected: String = "__PART_1_TEST_EXPECTED__",
      part2TestInput: String = "__PART_2_TEST_INPUT__",
      part2TestExpected: String = "__PART_2_TEST_EXPECTED__",
      part1Expected: String = "__PART_1_EXPECTED__",
      part2Expected: String = "__PART_2_EXPECTED__"
  ): Path =
    val dayStr = getDayStr(day)
    val templatePath =
      Root.resolve(
        Paths.get(
          TemplateRoot.toString,
          "DAY__DAY_STR__Test.scala"
        )
      )
    val template = readUtf8File(templatePath)
    val contents = TemplateFiller(template)
      .fill("__DAY_STR__", dayStr)
      .fill("__PART_1_TEST_INPUT__", part1TestInput)
      .fill("__PART_1_TEST_EXPECTED__", part1TestExpected)
      .fill("__PART_2_TEST_INPUT__", part2TestInput)
      .fill("__PART_2_TEST_EXPECTED__", part2TestExpected)
      .fill("__PART_1_EXPECTED__", part1Expected)
      .fill("__PART_2_EXPECTED__", part2Expected)
      .toString
    val solutionPath =
      Root.resolve(Paths.get(TestRoot.toString, s"Day${dayStr}Test.scala"))
    writeToUtf8File(solutionPath, contents)

  def createIndexFile() =
    val classNameList =
      import scala.jdk.CollectionConverters._
      Files
        .list(Root.resolve(SolutionRoot))
        .iterator
        .asScala
        .filter(Files.isRegularFile(_))
        .flatMap(x =>
          DayFileClassNameRegex
            .findFirstMatchIn(x.getFileName.toString)
            .map(_.group(1))
        )
        .toSeq

    // Get template for Index file
    val templatePath = Root.resolve(
      Paths.get(TemplateRoot.toString(), "Index.scala")
    )
    val template = readUtf8File(templatePath)

    // Fill template with class list and save it
    val contents = TemplateFiller(template)
      .fill("__SOLUTION_CLASS_LIST__", classNameList.map(x => s"classOf[$x]"))
      .toString
    val resultPath = Root.resolve(
      Paths.get(SourcePart.toString(), NamespacePart.toString, "solution", "Index.scala")
    )
    writeToUtf8File(resultPath, contents)

  def writeToUtf8File(fileName: Path, contents: String) =
    Option(fileName.getParent).map(Files.createDirectories(_))
    println(s"Writing $fileName")
    Files.writeString(fileName, contents, UTF_8)

  def readUtf8File(fileName: Path): String = Files.readString(fileName, UTF_8)

  def getDayStr(day: Int) = if (day < 10) s"0$day" else day.toString
