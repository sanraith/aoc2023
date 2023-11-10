package hu.sanraith.aoc2023

import hu.sanraith.aoc2023.cli._
import hu.sanraith.aoc2023.solution._
import java.nio.file.Paths
import java.util.Calendar

@main
def main(args: String*) =
  // clearConsole
  println("━━━━━━━━━━━━━━━━━━━")
  println("Advent of Code 2023")
  println("━━━━━━━━━━━━━━━━━━━")

  val dayRegex = """(\d+)""".r
  val firstArg = args.headOption.map(_.toLowerCase)
  firstArg match
    case Some("all") | None =>
      println("Solving all available days...")
      solveDays(solutionMap.keySet.toSeq.sorted)
    case Some("last") =>
      println("Solving last available day...")
      solveDays(Seq(solutionMap.keySet.toSeq.sorted.last))
    case Some("scaffold")           => scaffold(getDays(args.drop(1)))
    case Some("day") | Some("days") => solveDays(getDays(args.drop(1)))
    case Some(dayRegex(day))        => solveDays(getDays(args))
    case Some(_) =>
      println(s"Unknown parameter sequence: ${args.mkString(", ")}")
  println

def clearConsole = print("\u001b[2J")

def getDays(seq: Seq[String]): Seq[Int] = seq.flatMap(_.toIntOption)

def plural(seq: Seq[Any]): String = if (seq.length > 1) "s" else ""

def scaffold(days: Seq[Int]): Unit =
  val resolvedDays = FileManager.readSessionKey() match
    case None =>
      println(s"Please fill session key in ${FileManager.SESSION_KEY_FILENAME}")
      FileManager.writeToUtf8File(
        Paths.get(FileManager.SESSION_KEY_FILENAME),
        "YOUR_SESSION_KEY_HERE"
      )
      Seq()

    case Some(sessionKey) =>
      val resolvedDays = days.length match
        case 0 => Seq(Calendar.getInstance().get(Calendar.DAY_OF_MONTH))
        case _ => days

      println(s"Scaffolding day${plural(days)} ${days.mkString(", ")}...")
      resolvedDays.foreach: day =>
        WebClient(sessionKey).requestCached(s"2022/day/$day") match
          case None => println(s"Unable to scaffold day $day")
          case Some(body) =>
            FileManager.createSolutionFile(day, "Unknown Title") // TODO get title
            FileManager.createTestFile(day) // TODO get test input, expected result
      FileManager.createIndexFile()

def solveDays(days: Seq[Int]) =
  for (day <- days)
    solutionMap.get(day) match
      case Some(solutionInfo) =>
        val solution = solutionMap(day).createInstance()
        println(s"\n--- Day $day: ${solution.title} ---")
        SolutionRunner.run(solution)
      case None => println(s"\nNo solution found for day $day!")
