package hu.sanraith.aoc2023.solution

import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/21 */
class Day21 extends Solution:
  val enableDebugPrint = false
  var part1MaxSteps = 64
  var part2MaxSteps = 26501365

  override val title: String = "Step Counter"

  override def part1(ctx: Context): Int =
    val (start, layout) = parseLayout(ctx.input)
    countReachable(Seq((start, 0)), part1MaxSteps, layout)

  // Non-general solution using the quadratic properties of the repeating pattern
  override def part2(ctx: Context): Long =
    val (start, layout) = parseLayout(ctx.input)
    val maxSteps = part2MaxSteps
    val step = layout.width
    val Seq(b0, b1, b2) = (0 to 2)
      .map(i => start.x + step * i)
      .map(countReachableRepeating(Seq((start, 0)), _, layout)._1.toLong)
      .prepended(0L)
      .sliding(2)
      .map { case Seq(a, b) => b - a }
      .toSeq
    val n = part2MaxSteps / step.toLong
    return b0 + b1 * n + (n * (n - 1) / 2) * (b2 - b1)

  def countReachable(starts: Seq[(Point, Int)], maxSteps: Int, layout: Layout) =
    val queue = mut.Queue(starts: _*)
    val visited = mut.Map.empty[Point, Int]
    while (queue.length > 0) do
      val (pos, stepCount) = queue.dequeue()
      if (!visited.contains(pos))
        visited.addOne((pos, stepCount))
        if (stepCount < maxSteps)
          Directions
            .map(pos + _)
            .filter(!layout.rocks.contains(_))
            .foreach(nextPos => queue.enqueue((nextPos, stepCount + 1)))
    val visitedByExactSteps = visited.filter((k, v) => v % 2 == maxSteps % 2)
    visitedByExactSteps.size

  def countReachableRepeating(starts: Seq[(Point, Int)], maxSteps: Int, layout: Layout) =
    val queue = mut.Queue(starts: _*)
    val visited = mut.Map.empty[Point, Int]
    while (queue.length > 0) do
      val (pos, stepCount) = queue.dequeue()
      if (!visited.contains(pos))
        visited.addOne((pos, stepCount))
        if (stepCount < maxSteps)
          Directions
            .map(pos + _)
            .filter(p => !layout.rocks.contains(normalizePoint(p, layout)))
            .foreach(nextPos => queue.enqueue((nextPos, stepCount + 1)))
    val visitedByExactSteps = visited.filter((k, v) => v % 2 == maxSteps % 2)
    (visitedByExactSteps.size, visited)

  def normalizePoint(p: Point, layout: Layout) =
    val Layout(_, width, height) = layout
    var Point(x, y) = p
    x = (x + (math.abs(x) / width + 1) * width) % width
    y = (y + (math.abs(y) / height + 1) * height) % height
    Point(x, y)

  def debugPrint(
      layout: Layout,
      path: Vector[(Point, Int)],
      maxSteps: Int,
      force: Boolean = false
  ): Unit =
    if (path.isEmpty)
      println("empty area")
      return
    else if (!force && !enableDebugPrint) return

    val minX = path.map((p, _) => p.x).min
    val minY = path.map((p, _) => p.y).min
    val maxX = path.map((p, _) => p.x).max
    val maxY = path.map((p, _) => p.y).max

    val pathMap = path.toMap
    for (y <- minY to maxY)
      val line = (minX to maxX).map: x =>
        val p = Point(x, y)
        val normP = normalizePoint(p, layout)
        pathMap.get(p) match
          case _
              if (normP.x == 0 || normP.x == layout.width - 1) && (normP.y != 0 && normP.y != layout.height - 1) =>
            '│'
          case _
              if (normP.y == 0 || normP.y == layout.height - 1) && (normP.x != 0 && normP.x != layout.width - 1) =>
            '─'
          case _
              if normP.x == 0 || normP.y == 0 || normP.x == layout.width - 1 || normP.y == layout.height - 1 =>
            '┼'
          case Some(v) => if (v % 2 == maxSteps % 2) 'O' else '░' // █▓▒░
          case _       => if (layout.rocks.contains(normP)) '.' /*'#'*/ else ' ' // '.'
      println(line.mkString)
    println("")
  end debugPrint

  def parseLayout(input: String) =
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    var start = Point(0, 0)
    val rocks = input.linesIterator.zipWithIndex
      .flatMap: (l, y) =>
        l.zipWithIndex
          .map((c, x) => Point(x, y) -> c)
          .tapEach((p, c) => if (c == 'S') start = p)
          .filter((_, c) => c == '#')
          .map((p, _) => p)
      .toSet
    (start, Layout(rocks, width, height))

  case class Layout(rocks: Set[Point], width: Int, height: Int)

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)
    def *(other: Point): Point = Point(x * other.x, y * other.y)
    def *(scalar: Int): Point = Point(x * scalar, y * scalar)
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs

  type Direction = Point
  val East: Direction = Point(1, 0)
  val South: Direction = Point(0, 1)
  val West: Direction = Point(-1, 0)
  val North: Direction = Point(0, -1)
  val Center: Direction = Point(0, 0)
  val Directions = Seq(East, South, West, North)
