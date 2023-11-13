package hu.sanraith.aoc2023.cli

import hu.sanraith.aoc2023.common._
import hu.sanraith.aoc2023.solution._

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object SolutionRunner:
  def run(solution: Solution) =
    val input = Util.loadInputFromFile(solution)
    val context = ConsoleContext(input)
    try
      runPart(1, solution, context)
      runPart(2, solution, context)
    catch case e => println(e)

  private def runPart(part: Int, solution: Solution, context: ConsoleContext): Unit =
    context.isCompleted = false
    context.progress = None

    val start = System.nanoTime()
    val workFuture = Future:
      val result =
        try
          part match
            case 1 => solution.part1(context)
            case _ => solution.part2(context)
        catch e => e.toString
      context.isCompleted = true
      (result, System.nanoTime())

    val sameLine = LineRewriter()
    val progressFuture = Future:
      while (!context.isCompleted)
        val time = timeStr(start, System.nanoTime())
        val percentage = context.progress.map(x => f" ${x * 100}%2.2f%%").getOrElse("")
        sameLine.print(s"Part $part... ($time)$percentage")
        Thread.sleep(75)

    val (result, end) = Await.result(workFuture, Duration.Inf)
    Try(Await.ready(progressFuture, Duration.create(5, TimeUnit.SECONDS)))

    val lineStart = s"Part $part (${timeStr(start, end)}): "
    val formattedResult = result match
      case Util.includesNewLineRegex(lines) =>
        lines.split("\n").mkString("\n".padTo(lineStart.length + 1, ' '))
      case line => line

    sameLine.println(s"$lineStart$formattedResult")
  end runPart

  def timeStr(start: Long, end: Long): String = s"${(end - start) / 1000000} ms"

/** Prints content to the same console line until println is called. */
class LineRewriter:
  var prevLength: Int = 0

  def print(line: String): Unit =
    Predef.print(s"\r${line.padTo(prevLength, ' ')}")
    prevLength = line.length

  def println(line: String): Unit =
    print(s"$line\n")

class ConsoleContext(val input: String) extends Context:
  var progress: Option[Double] = None
  var isCompleted: Boolean = false
  override def progress(value: Double): Unit = progress = Some(value)
