package hu.sanraith.aoc2023.solution

import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/21 */
class Day21 extends Solution:
  val enableDebugPrint = true
  var part1MaxSteps = 64

  override val title: String = "Step Counter"

  override def part1(ctx: Context): Int =
    val (start, layout) = parseLayout(ctx.input)

    val maxSteps = part1MaxSteps
    val queue = mut.Queue((start, 0))
    val visited = mut.Map.empty[Point, Int]
    while (queue.length > 0) do
      val (pos, stepCount) = queue.dequeue()
      if (!visited.contains(pos))
        visited.addOne((pos, stepCount))
        if (stepCount < maxSteps)
          Directions
            .map(pos + _)
            .filter(layout.tiles.contains)
            .foreach(nextPos => queue.enqueue((nextPos, stepCount + 1)))

    val visitedByExactSteps = visited.filter((k, v) => v % 2 == maxSteps % 2)
    debugPrint(layout, visitedByExactSteps.toVector)
    visitedByExactSteps.size

  override def part2(ctx: Context): String =
    ???

  def debugPrint(layout: Layout, path: Vector[(Point, Int)]): Unit =
    if (!enableDebugPrint) return
    val pathMap = path.toMap
    for (y <- 0 until layout.height)
      val line = (0 until layout.width).map: x =>
        val p = Point(x, y)
        pathMap.get(p) match
          case Some(_) => 'O'
          case _       => if (layout.tiles.contains(p)) '.' else '#'
      println(line.mkString)
    println("")
  end debugPrint

  def parseLayout(input: String) =
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    var start = Point(0, 0)
    val tiles = input.linesIterator.zipWithIndex
      .flatMap: (l, y) =>
        l.zipWithIndex
          .filter((n, _) => n != '#')
          .map: (n, x) =>
            val p = Point(x, y)
            if (n == 'S') start = p
            p
      .toSet
    (start, Layout(tiles, width, height))

  case class Layout(tiles: Set[Point], width: Int, height: Int)

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)
    def *(scalar: Int): Point = Point(x * scalar, y * scalar)
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs

  type Direction = Point
  val East: Direction = Point(1, 0)
  val South: Direction = Point(0, 1)
  val West: Direction = Point(-1, 0)
  val North: Direction = Point(0, -1)
  val NoDir: Direction = Point(0, 0)
  val Directions = Seq(East, South, West, North)
