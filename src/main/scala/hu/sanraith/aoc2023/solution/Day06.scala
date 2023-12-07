package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/6 */
class Day06 extends Solution:
  override val title: String = "Wait For It"

  override def part1(ctx: Context): Int =
    val (matches, _) = parseMatches(ctx.input)
    matches
      .map((time, record) => (1 until time).count(speed => ((time - speed) * speed) > record))
      .product

  override def part2(ctx: Context): Int =
    val (_, (time, record)) = parseMatches(ctx.input)
    var count = 0
    for (speed <- (1 until time.toInt))
      if (speed % 1_000_000 == 0)
        ctx.progress(speed / time.toDouble)
      if (((time - speed) * speed) > record)
        count += 1
    count

  def parseMatches(input: String) =
    val numberRegex = """(\d+)""".r
    val numbersPerLine = input.linesIterator.map(numberRegex.findAllIn(_).map(_.toInt).toSeq).toSeq
    numbersPerLine match
      case Seq(times, distances) =>
        (times.zip(distances), (times.mkString.toLong, distances.mkString.toLong))
      case _ => throw new Exception("invalid input")
