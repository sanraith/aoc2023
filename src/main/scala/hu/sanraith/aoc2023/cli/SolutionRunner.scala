package hu.sanraith.aoc2023.cli

import hu.sanraith.aoc2023.common._
import hu.sanraith.aoc2023.solution._

import java.util.concurrent._
import scala.collection.immutable.LazyList
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
    try
      runPart(1, solution, input)
      runPart(2, solution, input)
    catch case e => println(e)

  private def runPart(part: Int, solution: Solution, input: String): Unit =
    val context = ConsoleContext(input)
    val statusLine = LineRewriter()
    solution.println = msg => context.debugMessageQueue.add(msg.toString)

    val resultFuture = runPartAsync(part, solution, context)
    val progressFuture = showProgressAsync(part, context, statusLine)
    val combinedFuture = for
      x <- resultFuture
      _ <- progressFuture
    yield x
    val (result, duration) = Await.result(combinedFuture, Duration.Inf)

    val lineStart = s"Part $part (${timeStr(duration)}): "
    val formattedResult = result match
      case Util.includesNewLineRegex(lines) =>
        lines.split("\n").mkString("\n".padTo(lineStart.length + 1, ' '))
      case line => line
    statusLine.println(s"$lineStart$formattedResult")

  private def runPartAsync(part: Int, solution: Solution, context: ConsoleContext) = Future:
    val start = System.nanoTime()
    val result =
      try
        part match
          case 1 => solution.part1(context)
          case _ => solution.part2(context)
      catch e => e.toString
    context.isCompleted = true
    (result, System.nanoTime() - start)

  private def showProgressAsync(
      part: Int,
      context: ConsoleContext,
      statusLine: LineRewriter
  ): Future[Unit] = Future:
    val start = System.nanoTime()
    while (!context.isCompleted)
      val time = timeStr(start, System.nanoTime())
      val percentage = context.progress.map(x => f" ${x * 100}%2.2f%%").getOrElse("")
      context.printDebugMessages(statusLine)
      statusLine.print(s"Part $part... ($time)$percentage")
      Thread.sleep(75)
    context.printDebugMessages(statusLine)

  def timeStr(start: Long, end: Long): String = timeStr(end - start)
  def timeStr(duration: Long): String = s"${duration / 1000000} ms"

/** Prints content to the same console line until println is called. */
class LineRewriter:
  private var prevLength: Int = 0

  def print(line: String): Unit =
    Predef.print(s"\r${line.padTo(prevLength, ' ')}")
    prevLength = line.length

  def println(line: String): Unit =
    print(line)
    Predef.println
    prevLength = 0

class ConsoleContext(val input: String) extends Context:
  val debugMessageQueue: ConcurrentLinkedQueue[String] = ConcurrentLinkedQueue()
  var progress: Option[Double] = None
  var isCompleted: Boolean = false

  def printDebugMessages(statusLine: LineRewriter) = LazyList
    .from(1)
    .map(_ => debugMessageQueue.poll)
    .takeWhile(x => x != null)
    .foreach(statusLine.println)

  override def progress(value: Double): Unit = progress = Some(value)
