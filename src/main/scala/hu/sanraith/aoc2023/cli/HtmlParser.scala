package hu.sanraith.aoc2023.cli

import org.jsoup.Jsoup
import scala.jdk.CollectionConverters._

class PuzzlePageContents(
    val title: String = "",
    val part1TestInput: String = "",
    val part1TestExpected: String = ""
)

object HtmlParser:
  def parsePuzzlePage(documentStr: String): Option[PuzzlePageContents] =
    val document = Jsoup.parse(documentStr)
    val titleOption =
      """.*: (.*) ---""".r.findFirstMatchIn(document.select("h2").first().text()).map(_.group(1))

    // Take the first block that has an 'example' sentence before it, or the first one without if none found
    val exampleInputCandidates = document
      .select("article:first-of-type pre code")
      .asScala
      .flatMap(elem =>
        elem
          .parent()
          .previousElementSiblings()
          .asScala
          .filter(!_.children.isEmpty)
          .headOption
          .map(_.text)
          .map(x => (x.toLowerCase.contains("example"), elem.textNodes().asScala.mkString("")))
      )
      .toList

    val testInput = exampleInputCandidates
      .collect { case (true, text) => text }
      .headOption
      .orElse(exampleInputCandidates.headOption.map { case (_, text) => text })
      .getOrElse("")

    // Take the last block that does not end with a question
    val endsWithQuestionRegex = """^.*\?\s*$""".r
    val testExpected = document
      .select("article:first-of-type em")
      .asScala
      .map(_.text.trim)
      .filter(endsWithQuestionRegex.findFirstMatchIn(_).isEmpty)
      .lastOption
      .getOrElse("")

    titleOption.map(title =>
      PuzzlePageContents(
        title = title,
        part1TestInput = testInput,
        part1TestExpected = testExpected
      )
    )
