package hu.sanraith.aoc2023.cli

import sys.process._
import scala.util.Try
import hu.sanraith.aoc2023.common._

class Scaffolder(sessionKey: String):
  def scaffoldDay(day: Int): Unit =
    val client = WebClient(sessionKey)
    (
      client.requestCached(s"${Util.currentYear}/day/$day/input"),
      client.requestCached(s"${Util.currentYear}/day/$day").map(HtmlParser.parsePuzzlePage).flatten
    ) match
      case (Some(input), Some(puzzle)) =>
        val inputPath = FileManager.createInputFile(day, input)
        val part1TestInput = Util.includesNewLineRegex.matches(puzzle.part1TestInput) match
          case true  => s"\n${puzzle.part1TestInput.trim}"
          case false => puzzle.part1TestInput
        val testPath = FileManager.createTestFile(
          day,
          part1TestInput = part1TestInput,
          part1TestExpected = puzzle.part1TestExpected
        )
        val solutionPath = FileManager.createSolutionFile(day, title = puzzle.title)

        println("Opening puzzle files in VS Code...")
        Try(s"code $inputPath $testPath $solutionPath".!)

      case _ => println(s"Unable to scaffold day $day")
