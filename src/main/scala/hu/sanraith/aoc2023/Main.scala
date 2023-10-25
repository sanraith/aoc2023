package hu.sanraith.aoc2023

import hu.sanraith.aoc2023.solution._

val testMacroVal = findDirectSubModules[Solution]

@main
def main(args: String*) = {
  // clearConsole
  println("━━━━━━━━━━━━━━━━━━━")
  println("Advent of Code 2023")
  println("━━━━━━━━━━━━━━━━━━━")

  val dayRegex = """(\d+)""".r
  val firstArg = args.headOption.map(_.toLowerCase)
  firstArg match {
    case Some("all") | None         => println("Solving all days")
    case Some("last")               => println("Solving last available day...")
    case Some("scaffold")           => scaffold(getDays(args.drop(1)))
    case Some("day") | Some("days") => solveDays(getDays(args.drop(1)))
    case Some(dayRegex(day))        => solveDays(getDays(args))
    case Some(_) =>
      println(s"Unknown parameter sequence: ${args.mkString(", ")}")
  }

  println
}

def clearConsole = print("\u001b[2J")

def getDays(seq: Seq[String]): Seq[Int] = seq.flatMap(_.toIntOption)

def plural(seq: Seq[Any]): String = if (seq.length > 1) "s" else ""

def scaffold(days: Seq[Int]) = {
  if (days.length > 0) {
    println(s"Scaffolding day${plural(days)} ${days.mkString(", ")}...")
  } else {
    println(s"Scaffolding missing days...")
  }
}

def solveDays(days: Seq[Int]) = {
  println(s"Solving day${plural(days)} ${days.mkString(", ")}...")
}
