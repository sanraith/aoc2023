package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/2 */
class Day02 extends Solution:
  override val title: String = "Cube Conundrum"

  override def part1(ctx: Context): String =
    val bag = Map("red" -> 12, "green" -> 13, "blue" -> 14)
    parseGames(ctx.input)
      .filter(_.hands.forall(_.forall((color, count) => count <= bag(color))))
      .map(_.id)
      .sum
      .toString

  override def part2(ctx: Context): String =
    parseGames(ctx.input)
      .map: game =>
        game.hands
          .fold(Map.empty)((acc, hand) =>
            acc ++ hand.map((color, count) => (color, acc.getOrElse(color, 0).max(count)))
          )
          .map((_, count) => count)
          .product
      .sum
      .toString

  def parseGames(input: String) =
    val handRegex = """(\d+) (\w+)""".r
    input.linesIterator.zipWithIndex
      .map: (line, i) =>
        val hands = line
          .split(";")
          .map(handRegex.findAllMatchIn(_).map(m => (m.group(2), m.group(1).toInt)).toMap)
        Game(i + 1, hands)

case class Game(id: Int, hands: Seq[Map[String, Int]])
