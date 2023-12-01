package hu.sanraith.aoc2023.solution

import scala.util.matching.Regex

/** Solution for https://adventofcode.com/2023/day/1 */
class Day01 extends Solution:
  override val title: String = "Trebuchet?!"

  override def part1(ctx: Context): String =
    val digitRegex = """\d""".r
    ctx.input.linesIterator
      .map(l => digitRegex.findAllIn(l).map(_.toInt).toSeq)
      .map(digits => digits.head * 10 + digits.last)
      .sum
      .toString

  override def part2(ctx: Context): String =
    val digitNames = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    val firstRegex = Regex("""\d|""" + digitNames.mkString("|"))
    val lastRegex = Regex(s".*($firstRegex)")
    val digitToInt = (digitNames.zip(1 to 9) ++ (0 to 9).map(_.toString).zipWithIndex).toMap

    ctx.input.linesIterator
      .map(l => Seq(firstRegex.findFirstIn(l), lastRegex.findFirstMatchIn(l).map(_.group(1))))
      .map(_.flatten.map(digitToInt))
      .map(digits => digits.head * 10 + digits.last)
      .sum
      .toString
