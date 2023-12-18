package hu.sanraith.aoc2023.solution

import hu.sanraith.aoc2023.util._
import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/18 */
class Day18 extends Solution:
  override val title: String = "Lavaduct Lagoon"

  override def part1(ctx: Context): Long =
    val commands = parseCommands(ctx.input)
    calcArea(commands, ctx)

  override def part2(ctx: Context): Long =
    val directions = Seq(East, South, West, North)
    val commands = parseCommands(ctx.input).map { case Command(_, _, color) =>
      val length = Integer.parseInt(color.take(color.length - 1), 16)
      val direction = directions(color.last.toString.toInt)
      Command(direction, length, color)
    }
    calcArea(commands, ctx)

  def calcArea(commands: Seq[Command], ctx: Context) =
    // Convert commands to points
    val (points, _) = commands.foldLeft((Seq.empty[Point], Point(0, 0))):
      case ((points, pos), c) =>
        val nextPos = pos + c.direction * c.length
        (points :+ nextPos, nextPos)

    // add each coord's next neighbour too
    val dPoints = points.flatMap(p => Seq(p, p + Point(1, 1)))
    // create rectangles of (topLeft, bottomRight) where each point has the same state
    val rects = (for
      xp <- dPoints.map(_.x).toSet.toSeq.sorted.sliding(2).toSeq
      yp <- dPoints.map(_.y).toSet.toSeq.sorted.sliding(2).toSeq
    yield (xp, yp)).map { case (Seq(x1, x2), Seq(y1, y2)) => Point(x1, y1) -> Point(x2, y2) }

    val pointPairs = (points.last +: points.take(points.length - 1)).zip(points)
    val horizontalEdges = pointPairs
      .filter((p1, p2) => p1.y == p2.y)
      .map((p1, p2) => (p1, p2, if (p1.x < p2.x) East else West))
      .sortBy(_._1.y)
    val verticalEdges = pointPairs
      .filter((p1, p2) => p1.x == p2.x)
      .map((p1, p2) => (p1, p2, if (p1.y < p2.y) South else North))
      .sortBy(_._1.x)

    val rectCount = rects.length.toDouble
    rects.iterator
      .tapEachWithIndex((_, i) => ctx.progress(i / rectCount))
      .filter { case (Point(x, y), _) =>
        // find rectangles where topLeft either:
        // - is on vertical edge
        // - is on horizontal edge
        // - the first and last vertical edge to the left has the same direction
        lazy val leftEdgeDirs = verticalEdges
          .takeWhile((a, _, _) => a.x < x)
          .filter((a, b, d) => (a.y to b.y by d.y).contains(y))
          .map((_, _, dir) => dir)
        horizontalEdges.exists((a, b, d) => y == a.y && (a.x to b.x by d.x).contains(x)) ||
        verticalEdges.exists((a, b, d) => x == a.x && (a.y to b.y by d.y).contains(y)) ||
        (leftEdgeDirs.length > 0 && leftEdgeDirs.head == leftEdgeDirs.last)
      }
      .map((p1, p2) => math.abs((p2.x - p1.x) * (p2.y - p1.y)))
      .sum

  def parseCommands(input: String) =
    val lineRegex = """(\S) (-?\d+) \(#(.+)\)""".r
    val directionMap = Map("U" -> North, "R" -> East, "D" -> South, "L" -> West)
    input.linesIterator.map { case lineRegex(dir, length, color) =>
      Command(directionMap(dir), length.toInt, color)
    }.toSeq

  case class Command(direction: Direction, length: Int, color: String)

  case class Point(x: Long, y: Long):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)
    def *(scalar: Long): Point = Point(x * scalar, y * scalar)
    def manhattan(other: Point): Long = (x - other.x).abs + (y - other.y).abs

  type Direction = Point
  val East: Direction = Point(1, 0)
  val South: Direction = Point(0, 1)
  val West: Direction = Point(-1, 0)
  val North: Direction = Point(0, -1)
