package hu.sanraith.aoc2023

import hu.sanraith.aoc2023.solution._

class ConsoleContext(val input: String) extends Context:
  override def progress(value: Double): Unit = {}

object SolutionRunner:
  val includesNewLineRegex = """^([\s\S]*\n[\s\S]*)$""".r

  def run(solution: Solution) =
    val context = ConsoleContext("test input")
    try
      timed(1, () => solution.part1(context))
      timed(2, () => solution.part2(context))
    catch case e => println(e)

  def timed[T](part: Int, work: () => T): Unit =
    import scala.util.matching.Regex
    var start = System.nanoTime()
    val result =
      (try work().toString
      catch e => e.toString) match
        case includesNewLineRegex(x) => s"\n$x"
        case x                       => x
    val end = System.nanoTime()
    println(s"Part $part (${timeStr(start, end)}): $result")

  def timeStr(start: Long, end: Long): String = s"${(end - start) / 1000000} ms"
