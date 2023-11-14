package hu.sanraith.aoc2023.cli

import sys.process._
import scala.util.Try
import hu.sanraith.aoc2023.common._

class Scaffolder(sessionKey: String):
  def scaffoldDay(day: Int, onlyInputs: Boolean, invalidateCache: Boolean): Unit =
    val client = WebClient(sessionKey)
    val inputPath = client
      .requestCached(s"${Util.currentYear}/day/$day/input", invalidateCache)
      .map(FileManager.createInputFile(day, _))
    if (inputPath.isEmpty) println(s"Unable to scaffold inputs for day $day")

    if (!onlyInputs)
      client
        .requestCached(s"${Util.currentYear}/day/$day", invalidateCache)
        .map(HtmlParser.parsePuzzlePage)
        .flatten match
        case Some(puzzle) =>
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

        case _ => println(s"Unable to scaffold puzzle data for day $day")
