package hu.sanraith.aoc2023.cli

class PuzzlePageContents(
    val title: String = "",
    val part1TestInput: String = "",
    val part2TestExpected: String = ""
)

object HtmlParser:
  def parsePuzzlePage(body: String): Option[PuzzlePageContents] =
    // TODO
    Some(PuzzlePageContents())
