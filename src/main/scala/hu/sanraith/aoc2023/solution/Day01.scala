package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/1 */
class Day01 extends Solution:
  override val title: String = "Trebuchet?!"

  override def part1(ctx: Context): Int =
    val digitRegex = """\d""".r
    ctx.input.linesIterator
      .map(digitRegex.findAllIn(_).map(_.toInt).toSeq)
      .map(digits => digits.head * 10 + digits.last)
      .sum

  override def part2(ctx: Context): Int =
    val digitNames = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    val digitRegex = s"(?=(\\d|${digitNames.mkString("|")}))".r
    val digitToInt = (digitNames.zip(1 to 9) ++ (0 to 9).map(_.toString).zipWithIndex).toMap

    ctx.input.linesIterator
      .map(digitRegex.findAllMatchIn(_).map(_.group(1)))
      .map(_.map(digitToInt).toSeq)
      .map(digits => digits.head * 10 + digits.last)
      .sum
