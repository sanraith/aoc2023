package hu.sanraith.aoc2023.cli

import sys.process._
import scala.util.Try

class Scaffolder(sessionKey: String):
  def scaffoldDay(day: Int): Unit =
    val client = WebClient(sessionKey)
    (
      client.requestCached(s"2022/day/$day/input"),
      client.requestCached(s"2022/day/$day").map(HtmlParser.parsePuzzlePage).flatten
    ) match
      case (Some(input), Some(puzzle)) =>
        val inputPath = FileManager.createInputFile(day, input)
        val testPath = FileManager.createTestFile(
          day,
          part1TestInput = puzzle.part1TestInput,
          part1TestExpected = puzzle.part2TestExpected
        )
        val solutionPath = FileManager.createSolutionFile(day, title = puzzle.title)

        println("Opening puzzle files in VS Code...")
        Try(s"code $inputPath $testPath $solutionPath".!)

      case _ => println(s"Unable to scaffold day $day")
