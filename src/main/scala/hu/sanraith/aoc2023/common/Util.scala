package hu.sanraith.aoc2023.common

import hu.sanraith.aoc2023.solution.Solution

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import scala.concurrent.duration.Duration
import scala.util.matching.Regex

object Util:
  val includesNewLineRegex: Regex = """^([\s\S]*\n[\s\S]*)$""".r
  val classDayRegex: Regex = """^.*?(\d+)$""".r
  val currentYear: Int = 2022

  val DURATION_SCALES: Seq[(Long, String, Int)] = Seq(
    (60_000_000_000L, "min", 2),
    (1_000_000_000, "s", 3),
    (1_000_000, "ms", 0),
    (1_000, "μs", 0),
    (1, "ns", 0)
  )
  val MIN_DURATION_SCALE = DURATION_SCALES.last._1

  def loadInputFromFile(solution: Solution) =
    val className = solution.getClass().getName()
    className match
      case classDayRegex(dayStr) => Files.readString(Paths.get("input", s"Day$dayStr.txt"), UTF_8)
      case _                     => throw Error(s"Cannot find puzzle input for class $className")

  def timeStr(duration: Duration): String = timeStr(duration.toNanos)
  def timeStr(start: Long, end: Long): String = timeStr(end - start)
  def timeStr(nanos: Long): String =
    DURATION_SCALES
      .flatMap: (scale, suffix, maxDecimals) =>
        val scaled = (nanos.toDouble) / scale
        if (scaled >= 1.0 || scale == MIN_DURATION_SCALE)
          val decimals = (maxDecimals - Math.log10(scaled).toInt).max(0)
          Some(s"%.${decimals}f $suffix".format(scaled))
        else None
      .head
