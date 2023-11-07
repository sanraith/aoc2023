package hu.sanraith.aoc2023

import hu.sanraith.aoc2023.solution._

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

def scaffold(days: Seq[Int]) =
  if (days.length > 0)
    println(s"Scaffolding day${plural(days)} ${days.mkString(", ")}...")
  else
    println(s"Scaffolding missing days...") // TODO handle missing days

  days.foreach: day =>
    FileGenerator.generateSolutionFile(day, "Unknown Title") // TODO get title
    FileGenerator.generateTestFile(day) // TODO get test input, expected result
  FileGenerator.generateIndexFile()

def solveDays(days: Seq[Int]) =
  for (day <- days)
    solutionMap.get(day) match
      case Some(solutionInfo) =>
        val solution = solutionMap(day).createInstance()
        println(s"\nDay $day - ${solution.title}")
        SolutionRunner.run(solution)
      case None => println(s"\nNo solution found for day $day!")
