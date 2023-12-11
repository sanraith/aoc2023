package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/11 */
class Day11 extends Solution:
  override val title: String = "Cosmic Expansion"

  override def part1(ctx: Context): Long =
    val (galaxies, width, height) = parseInput(ctx.input)
    val expanded = expand(galaxies, width, height)
    expanded.combinations(2).map { case Seq(a, b) => a.manhattan(b) }.sum

  override def part2(ctx: Context): Long =
    val (galaxies, width, height) = parseInput(ctx.input)
    val expanded = expand(galaxies, width, height, 999999)
    expanded.combinations(2).map { case Seq(a, b) => a.manhattan(b) }.sum

  def expand(source: Seq[Point], width: Int, height: Int, amount: Long = 1) =
    val wider = (1 until width)
      .filter(x => source.forall(_.x != x))
      .foldLeft(source): (points, x) =>
        points.map(p => if (p.x < x) Point(p.x - amount, p.y) else p)
    (1 until height) // taller
      .filter(y => wider.forall(_.y != y))
      .foldLeft(wider): (points, y) =>
        points.map(p => if (p.y < y) Point(p.x, p.y - amount) else p)

  def parseInput(input: String) =
    val galaxies = input.linesIterator.zipWithIndex.flatMap: (line, y) =>
      line.zipWithIndex.filter((c, _) => c == '#').map((c, x) => Point(x, y))
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    (galaxies.toSeq, width, height)

  case class Point(x: Long, y: Long):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)
    def manhattan(other: Point): Long = (x - other.x).abs + (y - other.y).abs
