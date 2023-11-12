package hu.sanraith.aoc2023.cli

import hu.sanraith.aoc2023.solution._
import hu.sanraith.aoc2023.common._

class ConsoleContext(val input: String) extends Context:
  override def progress(value: Double): Unit = {}

object SolutionRunner:
  def run(solution: Solution) =
    val input = Util.loadInputFromFile(solution)
    val context = ConsoleContext(input)
    try
      timed(1, () => solution.part1(context))
      timed(2, () => solution.part2(context))
    catch case e => println(e)

  def timed[T](part: Int, work: () => String): Unit =
    var start = System.nanoTime()
    val result =
      try work()
      catch e => e.toString
    val end = System.nanoTime()

    val lineStart = s"Part $part (${timeStr(start, end)}): "
    val formattedResult = result match
      case Util.includesNewLineRegex(lines) =>
        lines.split("\n").mkString("\n".padTo(lineStart.length + 1, ' '))
      case line => line

    println(s"$lineStart$formattedResult")

  def timeStr(start: Long, end: Long): String = s"${(end - start) / 1000000} ms"
