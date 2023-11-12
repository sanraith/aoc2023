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
    val example_input_candidates = document
      .select("article:first-of-type pre code")
      .asScala
      .map(elem =>
        elem
          .parent()
          .previousElementSiblings()
          .asScala
          .filter(!_.children.isEmpty)
          .headOption
          .map(_.textNodes.asScala.mkString(" "))
          .map(x => (x.toLowerCase.contains("example"), elem.textNodes().asScala.mkString("")))
      )
      .flatten
      .toList

    val test_input =
      example_input_candidates
        .filter { case (hasExample, _) => hasExample }
        .headOption
        .map { case (_, text) => text }
        .orElse(
          example_input_candidates
            .lift(0)
            .map { case (_, text) => text }
        )
        .getOrElse("")

    // Take the last block that does not end with a question
    val ends_with_question_regex = """^.*\?\s*$""".r
    val test_expected = document
      .select("article:first-of-type em")
      .asScala
      .map(_.textNodes.asScala.mkString(" "))
      .filter(ends_with_question_regex.findFirstMatchIn(_).isEmpty)
      .lastOption
      .map(_.trim)
      .getOrElse("")

    titleOption.map(title =>
      PuzzlePageContents(
        title = title,
        part1TestInput = test_input,
        part1TestExpected = test_expected
      )
    )

  end parsePuzzlePage
