package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/24 */
class Day24 extends Solution:

  var part1XYRange = 200000000000000L to 400000000000000L

  override val title: String = "Never Tell Me The Odds"

  override def part1(ctx: Context): Int =
    val stones = parseInput(ctx.input)
    val tRange = part1XYRange
    stones.combinations(2).count { case Seq(sa, sb) =>
      intersection(sa, sb) match
        case None => false
        case Some((x, y)) =>
          tRange.start <= x && x <= tRange.end && tRange.start <= y && y <= tRange.end
    }

  override def part2(ctx: Context): Long =
    val stones = parseInput(ctx.input)
    ???

  def intersection(sa: HailStone, sb: HailStone): Option[(Double, Double)] =
    val Point(x1, y1, _) = sa.pos
    val Point(x2, y2, _) = sa.pos + sa.vel
    val Point(x3, y3, _) = sb.pos
    val Point(x4, y4, _) = sb.pos + sb.vel
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
    val d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    val t0 = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
    val u0 = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)
    if (d == 0) // parallel
      if (t0 == 0 && u0 == 0) ??? // overlapping
      else None // not overlapping
    else if (
      (0 <= t0 && 0 <= d || 0 <= -t0 && 0 <= -d) && (0 <= u0 && 0 <= d || 0 <= -u0 && 0 <= -d)
    )
      val t = t0 / d.toDouble
      val x = x1 + t * (x2 - x1)
      val y = y1 + t * (y2 - y1)
      Some((x, y))
    else None

  def parseInput(input: String) =
    val numberRegex = """-?\d+""".r
    input.linesIterator
      .map: line =>
        val Seq(px, py, pz, vx, vy, vz) = numberRegex.findAllIn(line).map(_.toLong).toList
        HailStone(Point(px, py, pz), Point(vx, vy, vz))
      .toSeq

  case class HailStone(pos: Point, vel: Point)

  case class Point(x: Long, y: Long, z: Long):
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
    def -(other: Point): Point = Point(x - other.x, y - other.y, z - other.z)
    def *(factor: Long): Point = Point(x * factor, y * factor, z * factor)
