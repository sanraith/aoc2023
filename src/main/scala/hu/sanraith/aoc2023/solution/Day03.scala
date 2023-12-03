package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/3 */
class Day03 extends Solution:
  override val title: String = "Gear Ratios"

  override def part1(ctx: Context): String =
    parseParts(ctx.input).flatMap(_.numbers).sum.toString

  override def part2(ctx: Context): String =
    parseParts(ctx.input)
      .filter(p => p.symbol == "*" && p.numbers.length == 2)
      .map(_.numbers.product)
      .sum
      .toString

  def parseParts(input: String) =
    val numberRegex = """\d+""".r
    val numbers = input.linesIterator.zipWithIndex
      .flatMap: (line, i) =>
        numberRegex.findAllMatchIn(line).map(m => ((m.start until m.end), i, m.matched.toInt))
      .toSeq // (rangeX, y, number)

    val symbolRegex = """[^\.\d\n]""".r
    input.linesIterator.zipWithIndex
      .flatMap((line, i) => symbolRegex.findAllMatchIn(line).map(m => (m.start, i, m.matched)))
      .map: (x, y, symbol) =>
        val symRangeX = (x - 1) to (x + 1)
        val symRangeY = (y - 1) to (y + 1)
        val partNumbers = numbers
          .filter((numRX, numY, _) => symRangeX.intersects(numRX) && symRangeY.contains(numY))
          .map((_, _, num) => num)
        Part(symbol, partNumbers)

  case class Part(symbol: String, numbers: Seq[Int])

  implicit class RangeOps(r1: Range):
    def intersects(r2: Range): Boolean =
      r1.contains(r2.start) || r1.contains(if (r2.isInclusive) r2.end else r2.end - 1)
