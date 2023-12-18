package hu.sanraith.aoc2023.solution

import hu.sanraith.aoc2023.util._
import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/18 */
class Day18 extends Solution:
  val enableDebugPrint = false

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
    // Normalize coordinates
    val (minPos, maxPos, _) = commands.foldLeft(Point(0, 0), Point(0, 0), Point(0, 0)):
      case ((min, max, pos), c) =>
        (
          Point(math.min(pos.x, min.x), math.min(pos.y, min.y)),
          Point(math.max(pos.x, max.x), math.max(pos.y, max.y)),
          pos + c.direction * c.length
        )
    val start = minPos * -1
    val bottomRight = maxPos - minPos

    val tiles: mut.Map[Point, Long] = mut.Map.empty
    var pos = start
    val points = commands.map: c =>
      pos += c.direction * c.length
      pos

    val dPoints = points.flatMap(p => Seq(p, p + Point(1, 1)))
    var rectId = 0
    val rects = (for
      xp <- dPoints.map(_.x).toSet.toSeq.sorted.sliding(2).toSeq
      yp <- dPoints.map(_.y).toSet.toSeq.sorted.sliding(2).toSeq
    yield (xp, yp)).map:
      case (Seq(x1, x2), Seq(y1, y2)) =>
        if (enableDebugPrint)
          (x1 until x2)
            .flatMap(x => (y1 until y2).map(y => Point(x, y)))
            .foreach(p => tiles.addOne(p, rectId))
          rectId += 1
          // debugPrint(tiles, bottomRight + Point(1, 1))
        Point(x1, y1) -> Point(x2, y2)
    // debugPrint(tiles, bottomRight + Point(1, 1))

    val pointPairs = (Seq(points.last) ++ points.take(points.length - 1)).zip(points)
    val horizontalEdges = pointPairs
      .filter((p1, p2) => p1.y == p2.y)
      .map((p1, p2) => (p1, p2, if (p1.x < p2.x) East else West))
      .sortBy(_._1.y)
    val verticalEdges = pointPairs
      .filter((p1, p2) => p1.x == p2.x)
      .map((p1, p2) => (p1, p2, if (p1.y < p2.y) South else North))
      .sortBy(_._1.x)

    val rectCount = rects.length.toDouble
    val area = rects.iterator
      .tapEachWithIndex((_, i) => ctx.progress(i / rectCount))
      .map((p1, p2) => {
        // a. is on vertical edge
        // b. is on horizontal edge
        // c. has odd number of vertical edges on its left only counting (a until b) of the edge
        val Point(x, y) = p1
        val isIncluded =
          horizontalEdges.exists((a, b, d) => y == a.y && (a.x to b.x by d.x).contains(x)) ||
            verticalEdges.exists((a, b, d) => x == a.x && (a.y to b.y by d.y).contains(y)) || {
              val leftEdgeDirs = verticalEdges
                .takeWhile((a, _, _) => a.x < x)
                .filter((a, b, d) => (a.y to b.y by d.y).contains(y))
                .map((_, _, d) => d)
              (leftEdgeDirs.length > 0 && leftEdgeDirs.head == leftEdgeDirs.last)
            }

        if (enableDebugPrint)
          (p1.x until p2.x)
            .flatMap(x => (p1.y until p2.y).map(y => Point(x, y)))
            .foreach(p => tiles.addOne(p, if (isIncluded) 1 else 0))

        if (isIncluded) math.abs((p2.x - p1.x) * (p2.y - p1.y))
        else 0
      })
      .sum
    if (enableDebugPrint)
      debugPrint(tiles, bottomRight + Point(1, 1))
      (Seq(points.last) ++ points.take(points.length - 1))
        .zip(commands)
        .filter((p, c) => p.y == 0)
        .foreach((p, c) => println(s"$p: $c"))

    area

  def debugPrint(
      tiles: mut.Map[Point, Long],
      size: Point
  ): Unit =
    if (!enableDebugPrint) return
    val Point(width, height) = size
    for (y <- 0 until height.toInt)
      val line = (0 until width.toInt).map: x =>
        val p = Point(x, y)
        if (tiles.contains(p)) tiles(p).toString.reverse.padTo(1, ' ').reverse
        else "."
      println(line.mkString)
    println("")
  end debugPrint

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
