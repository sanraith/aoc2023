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
    case Some("scaffold")    => scaffold(args.drop(1))
    case Some("day")         => solveDays(getDays(args.drop(1)))
    case Some(dayRegex(day)) => solveDays(getDays(args))
    case Some(_) =>
      println(s"Unknown parameter sequence: ${args.mkString(", ")}")
  println

def clearConsole = print("\u001b[2J")

def getDays(seq: Seq[String]): Seq[Int] = seq.flatMap(_.toIntOption)

def plural(seq: Seq[Any]): String = if (seq.length > 1) "s" else ""

def scaffold(
    args: Seq[String],
    sessionKey: Option[String] = None,
    onlyInputs: Boolean = false,
    invalidateCache: Boolean = false
): Unit =
  sessionKey.orElse(FileManager.readSessionKey()) match
    case None =>
      println(s"Please fill session key in ${FileManager.SESSION_KEY_FILENAME}")
      FileManager.writeToUtf8File(
        Paths.get(FileManager.SESSION_KEY_FILENAME),
        "YOUR_SESSION_KEY_HERE"
      )

    case Some(sessionKey) =>
      args.headOption.map(_.toLowerCase) match
        case Some("reload") =>
          scaffold(args.drop(1), Some(sessionKey), onlyInputs, invalidateCache = true)
        case Some("input") =>
          scaffold(args.drop(1), Some(sessionKey), onlyInputs = true, invalidateCache)
        case Some("inputs") =>
          scaffold(
            solutionMap.keySet.toSeq.sorted.map(_.toString),
            onlyInputs = true,
            invalidateCache
          )
        case _ =>
          val days = getDays(args)
          val scaffolder = Scaffolder(sessionKey)
          val resolvedDays = days.length match
            case 0 =>
              Seq(
                solutionMap.keySet.toSeq.sorted.lastOption
                  .map(day => Math.min(day + 1, 25))
                  .getOrElse(1)
              )
            case _ => days

          println(s"Scaffolding day${plural(days)} ${days.mkString(", ")}...")
          resolvedDays.foreach(scaffolder.scaffoldDay(_, onlyInputs, invalidateCache))
          if (!onlyInputs) FileManager.createIndexFile()

def solveDays(days: Seq[Int]) =
  for (day <- days)
    solutionMap.get(day) match
      case Some(solutionInfo) =>
        val solution = solutionMap(day).createInstance()
        println(s"\n--- Day $day: ${solution.title} ---")
        SolutionRunner.run(solution)
      case None => println(s"\nNo solution found for day $day!")
