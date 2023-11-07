package hu.sanraith.aoc2023

import hu.sanraith.aoc2023.solution._
import org.scalatest.funspec.AnyFunSpec

import scala.util.matching.Regex

abstract class SolutionTestSpec extends AnyFunSpec {
  private val leadingNewLineRegex: Regex = """(?s)^\s*(?:\n|\r\n)(.*)$""".r
  private val classDayRegex: Regex = """^.*?(\d+)$""".r

  def assertPart(
      part: (Context) => String,
      expected: String,
      input: String = null
  )(implicit solution: Solution) =
    val resolvedInput = trim(Option(input).getOrElse(loadInputFromFile))
    val actual = part(TestContext(resolvedInput))
    assertResult(trim(expected))(actual)

  /** Pending assert */
  def _assertPart(
      part: (Context) => String,
      expected: String,
      input: String = null
  ) = pending

  private def loadInputFromFile(implicit solution: Solution) =
    classDayRegex.findFirstMatchIn(solution.getClass().getName()) match
      case None    => {}
      case Some(m) => println(s"asdasdasda ${m.group(1)}")
    "" // TODO load from file

  /** Trim the starting newline from a string. Allows easier-to-read declaration
    * with """..."""
    */
  private def trim(text: String): String =
    leadingNewLineRegex.findFirstMatchIn(text) match
      case Some(m) => m.group(1)
      case None    => text
}

private final class TestContext(val input: String) extends Context {
  override def progress(value: Double): Unit = {}
}
