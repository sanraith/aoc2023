package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/6 */
class Day06 extends Solution:
  override val title: String = "Wait For It"

  override def part1(ctx: Context): String =
    val (matches, _) = parseMatches(ctx.input)
    matches
      .map((time, record) => (1 until time).count(speed => ((time - speed) * speed) > record))
      .product
      .toString

  override def part2(ctx: Context): String =
    val (_, (time, record)) = parseMatches(ctx.input)
    var count = 0
    for (speed <- (1 until time.toInt))
      if (((time - speed) * speed) > record)
        count += 1

    count.toString

  def parseMatches(input: String) =
    val inputRegex = """(?s)Time:(?<time>.*)\s*Distance:(?<distance>.*)""".r
    val numberRegex = """(\d+)""".r
    input match
      case inputRegex(timesStr, distancesStr) =>
        val times = numberRegex.findAllIn(timesStr).map(_.toInt).toSeq
        val distances = numberRegex.findAllIn(distancesStr).map(_.toInt).toSeq
        (times.zip(distances), (times.mkString.toLong, distances.mkString.toLong))
      case _ => throw new Exception("invalid input")
