package hu.sanraith.aoc2023.solution

import hu.sanraith.aoc2023.util._
import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/16 */
class Day16 extends Solution:
  override val title: String = "The Floor Will Be Lava"

  override def part1(ctx: Context): Int =
    countEnergized(parseLayout(ctx.input), Point(0, 0), East)

  override def part2(ctx: Context): Int =
    val layout = parseLayout(ctx.input)
    val startPairs = for
      x <- 0 until layout.width
      y <- 0 until layout.height
      if x == 0 || x == layout.width - 1 || y == 0 || y == layout.height - 1
      direction <- (x, y) match
        case (0, y) if y > 0 && y < layout.height - 1                          => Seq(East)
        case (x, 0) if x > 0 && x < layout.width - 1                           => Seq(South)
        case (x, y) if y > 0 && y < layout.height - 1 && x == layout.width - 1 => Seq(West)
        case (x, y) if x > 0 && x < layout.width - 1 && y == layout.height - 1 => Seq(North)
        case _ => Seq(East, South, West, North)
    yield (Point(x, y), direction)

    startPairs.iterator
      .map((pos, dir) => countEnergized(layout, pos, dir))
      .tapEachWithIndex((_, i) => ctx.progress(i.toDouble / startPairs.size))
      .max

  def countEnergized(layout: Layout, startPoint: Point, startDir: Direction): Int =
    val energized: mut.Map[Point, mut.Set[Direction]] = mut.Map.empty
    val queue = mut.Queue((startPoint, startDir))
    while (queue.size > 0) do
      val (pos, dir) = queue.dequeue()
      val energizedDirs = energized.getOrElseUpdate(pos, mut.Set.empty)
      if (energizedDirs.add(dir))
        val nextDirs = layout.tiles
          .get(pos)
          .map(c => RedirectMap(c).get(dir).getOrElse(Seq(dir)))
          .getOrElse(Seq.empty)
        queue.addAll(nextDirs.map(d => (pos + d, d)).filter((p, _) => layout.tiles.contains(p)))
        // debugPrint(layout, energized)
    energized.size

  def debugPrint(layout: Layout, energized: Map[Point, Any]) =
    for (y <- 0 until layout.height)
      val line = (0 until layout.width)
        .map(x => Point(x, y))
        .map(p => if (energized.contains(p)) '█' else layout.tiles.get(p))
        .mkString
      println(line)

  def parseLayout(input: String) =
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    val tiles = input.linesIterator.zipWithIndex
      .flatMap((l, y) => l.zipWithIndex.map((c, x) => Point(x, y) -> c))
      .toMap
    Layout(tiles, width, height)

  case class Layout(tiles: Map[Point, Char], width: Int, height: Int)
  case class Point(x: Long, y: Long):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)

  type Direction = Point
  val East: Direction = Point(1, 0)
  val South: Direction = Point(0, 1)
  val West: Direction = Point(-1, 0)
  val North: Direction = Point(0, -1)
  val RedirectMap: Map[Char, Map[Direction, Seq[Direction]]] = Map(
    '/' -> Map(East -> Seq(North), South -> Seq(West), West -> Seq(South), North -> Seq(East)),
    '\\' -> Map(East -> Seq(South), South -> Seq(East), West -> Seq(North), North -> Seq(West)),
    '|' -> Map(East -> Seq(North, South), West -> Seq(North, South)),
    '-' -> Map(South -> Seq(West, East), North -> Seq(West, East)),
    '.' -> Map.empty
  )
