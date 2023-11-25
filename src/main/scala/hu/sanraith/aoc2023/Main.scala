package hu.sanraith.aoc2023

import hu.sanraith.aoc2023.cli._
import hu.sanraith.aoc2023.solution._

import java.nio.file.Paths
import java.util.Calendar
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

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
      solveDays(SolutionMap.keySet.toSeq.sorted)
    case Some("last") =>
      println("Solving last available day...")
      solveDays(Seq(SolutionMap.keySet.toSeq.sorted.last))
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
  sessionKey.orElse(AppConfig.instance.sessionCookie) match
    case None =>
      println(s"Please fill session cookie in ${AppConfig.ConfigFileName}")

    case Some(sessionKey) =>
      args.headOption.map(_.toLowerCase) match
        case Some("index") =>
          println("Scaffolding solution index...")
          FileManager.createIndexFile()
        case Some("reload") =>
          scaffold(args.drop(1), Some(sessionKey), onlyInputs, invalidateCache = true)
        case Some("input") =>
          scaffold(args.drop(1), Some(sessionKey), onlyInputs = true, invalidateCache)
        case Some("inputs") =>
          scaffold(
            SolutionMap.keySet.toSeq.sorted.map(_.toString),
            onlyInputs = true,
            invalidateCache
          )
        case _ =>
          val days = getDays(args)
          val scaffolder = Scaffolder(sessionKey)
          val resolvedDays = days.length match
            case 0 =>
              Seq(
                SolutionMap.keySet.toSeq.sorted.lastOption
                  .map(day => Math.min(day + 1, 25))
                  .getOrElse(1)
              )
            case _ => days

          println(s"Scaffolding day${plural(resolvedDays)} ${resolvedDays.mkString(", ")}...")
          resolvedDays.foreach(scaffolder.scaffoldDay(_, onlyInputs, invalidateCache))
          if (!onlyInputs) FileManager.createIndexFile()

def solveDays(days: Seq[Int]) =
  val totalDuration = days
    .map: day =>
      SolutionMap.get(day) match
        case Some(solutionInfo) =>
          val solution = solutionInfo.createInstance()
          println(s"\n--- Day $day: ${solution.title} ---")
          SolutionRunner.run(solution)
        case None => println(s"\nNo solution found for day $day!")
    .collect { case duration: FiniteDuration => duration }
    .fold(Duration.Zero)((a, x) => a + x)

  if (days.nonEmpty)
    println(s"\nTotal solution time: ${Util.timeStr(totalDuration)}")
  else
    println("\nNo solutions available!\nUse `run scaffold 1` to scaffold files for day 1.")
