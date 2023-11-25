package hu.sanraith.aoc2023.cli

import sys.process._
import scala.util.Try

class Scaffolder(sessionKey: String):
  def scaffoldDay(day: Int, onlyInputs: Boolean, invalidateCache: Boolean): Unit =
    val client = WebClient(sessionKey)
    val inputPath = client
      .requestCached(s"${Util.CurrentYear}/day/$day/input", invalidateCache)
      .map(FileManager.createInputFile(day, _))
    if (inputPath.isEmpty) println(s"Unable to scaffold inputs for day $day")

    if (!onlyInputs)
      client
        .requestCached(s"${Util.CurrentYear}/day/$day", invalidateCache)
        .map(HtmlParser.parsePuzzlePage)
        .flatten match
        case Some(puzzle) =>
          val part1TestInput = Util.IncludesNewLineRegex.matches(puzzle.part1TestInput) match
            case true  => s"\n${puzzle.part1TestInput.trim}"
            case false => puzzle.part1TestInput
          val testPath = FileManager.createTestFile(
            day,
            part1TestInput = part1TestInput,
            part1TestExpected = puzzle.part1TestExpected
          )
          val solutionPath = FileManager.createSolutionFile(day, title = puzzle.title)

          val prefs = AppConfig.instance
          (prefs.openScaffoldedFiles, prefs.pathToEditor) match
            case (true, Some(pathToEditor)) =>
              println("Opening puzzle files in editor...")
              val fileList =
                Seq(inputPath, Some(testPath), Some(solutionPath)).flatten.mkString(" ")
              Try(s"$pathToEditor $fileList".!)
            case _ => ()

        case _ => println(s"Unable to scaffold puzzle data for day $day")
