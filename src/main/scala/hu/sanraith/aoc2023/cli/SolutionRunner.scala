package hu.sanraith.aoc2023.cli

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
import scala.concurrent.duration.FiniteDuration

object SolutionRunner:
  def run(solution: Solution): FiniteDuration =
    val input = Util.loadInputFromFile(solution)
    var duration = Duration.Zero
    try
      duration += runPart(1, solution, input)
      duration += runPart(2, solution, input)
    catch case e => println(e)
    duration

  private def runPart(part: Int, solution: Solution, input: String): FiniteDuration =
    val context = ConsoleContext(input)
    val statusLine = LineRewriter()
    solution.println = msg => context.debugMessageQueue.add(msg.toString)

    val resultFuture = runPartAsync(part, solution, context)
    val progressFuture = showProgressAsync(part, context, statusLine)
    val combinedFuture = for
      x <- resultFuture
      _ <- progressFuture
    yield x
    val (result, duration) = Await.result(combinedFuture, Duration.Inf) match
      case (Success(result), duration) =>
        copyToClipboardIfNeeded(result)
        (result, duration)
      case (Failure(error), duration) => (error.toString, duration)

    val timeStr = s"(${Util.timeStr(duration)})".reverse.padTo(9, ' ').reverse
    val lineStart = s"Part $part $timeStr: "
    val formattedResult = result match
      case Util.IncludesNewLineRegex(lines) =>
        lines.split("\n").mkString("\n".padTo(lineStart.length + 1, ' '))
      case line => line
    statusLine.println(s"$lineStart$formattedResult")
    duration

  private def runPartAsync(
      part: Int,
      solution: Solution,
      context: ConsoleContext
  ): Future[(Try[String], FiniteDuration)] = Future:
    val start = System.nanoTime()
    val result = Try:
      part match
        case 1 => solution.part1(context)
        case _ => solution.part2(context)

    context.isCompleted = true
    val duration = Duration.fromNanos(System.nanoTime() - start)
    (result.map(_.toString), duration)

  private def showProgressAsync(
      part: Int,
      context: ConsoleContext,
      statusLine: LineRewriter
  ): Future[Unit] = Future:
    val start = System.nanoTime()
    while (!context.isCompleted)
      val time = Util.timeStr(start, System.nanoTime())
      val percentage = context.progress.map(x => f" ${x * 100}%2.2f%%").getOrElse("")
      context.printDebugMessages(statusLine)
      statusLine.print(s"Part $part... ($time)$percentage")
      Thread.sleep(75)
    context.printDebugMessages(statusLine)

  private def copyToClipboardIfNeeded(text: String): Unit =
    if (AppConfig.instance.copyResultToClipboard)
      Try:
        val clipboard = java.awt.Toolkit.getDefaultToolkit().getSystemClipboard();
        val content = new java.awt.datatransfer.StringSelection(text)
        clipboard.setContents(content, content);

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
    .takeWhile(_ != null)
    .foreach(statusLine.println)

  override def progress(value: Double): Unit = progress = Some(value)
