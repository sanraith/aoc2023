package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/3 */
class Day03 extends Solution:
  override val title: String = "Gear Ratios"

  override def part1(ctx: Context): Int =
    parseParts(ctx.input).flatMap(_.numbers).sum

  override def part2(ctx: Context): Int =
    parseParts(ctx.input)
      .filter(p => p.symbol == "*" && p.numbers.length == 2)
      .map(_.numbers.product)
      .sum

  def parseParts(input: String) =
    val numberRegex = """\d+""".r
    val numbersPerLine: Seq[List[(Range, Int)]] = input.linesIterator
      .map(numberRegex.findAllMatchIn(_).map(m => ((m.start until m.end), m.matched.toInt)).toList)
      .toSeq

    val symbolRegex = """[^\.\d\n]""".r
    input.linesIterator.zipWithIndex
      .flatMap((line, y) => symbolRegex.findAllMatchIn(line).map(m => (m.start, y, m.matched)))
      .map: (x, y, symbol) =>
        val symRangeX = (x - 1) to (x + 1)
        val symRangeY = (y - 1) to (y + 1)
        val partNumbers = numbersPerLine
          .slice(symRangeY.start, symRangeY.end + 1)
          .flatten
          .filter((numRangeX, _) => symRangeX.intersects(numRangeX))
          .map((_, num) => num)
        Part(symbol, partNumbers)

  case class Part(symbol: String, numbers: Seq[Int])

  implicit class RangeOps(r1: Range):
    def intersects(r2: Range): Boolean =
      r1.contains(r2.start) || r1.contains(if (r2.isInclusive) r2.end else r2.end - 1)
