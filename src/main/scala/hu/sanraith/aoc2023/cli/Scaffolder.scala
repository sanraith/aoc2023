package hu.sanraith.aoc2023.cli

class Scaffolder(sessionKey: String):
  def scaffoldDay(day: Int): Unit =
    val client = WebClient(sessionKey)
    (
      client.requestCached(s"2022/day/$day/input"),
      client.requestCached(s"2022/day/$day").map(HtmlParser.parsePuzzlePage).flatten
    ) match
      case (Some(input), Some(puzzle)) =>
        FileManager.createInputFile(day, input)
        FileManager.createSolutionFile(day, title = puzzle.title)
        FileManager.createTestFile(
          day,
          part1TestInput = puzzle.part1TestInput,
          part1TestExpected = puzzle.part2TestExpected
        )
      case _ => println(s"Unable to scaffold day $day")
