package hu.sanraith.aoc2023.solution

import scala.collection.mutable

/** Solution for https://adventofcode.com/2023/day/10 */
class Day10 extends Solution:
  override val title: String = "Pipe Maze"

  override def part1(ctx: Context): Int =
    val (loop, _) = findLoopFixTiles(parseTiles(ctx.input))
    loop.size / 2

  override def part2(ctx: Context): Int =
    val (loop, tiles) = findLoopFixTiles(parseTiles(ctx.input))
    val loopByY = loop.groupBy(_.pos.y).map((y, tiles) => (y, tiles.toSeq.sortBy(_.pos.x)))
    val loopSet = loop.toSet

    val cornerPairs = Map('F' -> '7', 'L' -> 'J')
    tiles.values
      .filter(!loopSet.contains(_))
      .count: t =>
        val leftWalls = loopByY.get(t.pos.y).getOrElse(Seq.empty).filter(_.pos.x < t.pos.x)
        val wallCount = leftWalls.foldLeft((0, '.')):
          case ((count, prevCorner), wall) =>
            wall.symbol match
              case '|'                                         => (count + 1, prevCorner)
              case c if cornerPairs.get(prevCorner) == Some(c) => (count - 1, c)
              case c: ('L' | 'F')                              => (count + 1, c)
              case c: ('7' | 'J')                              => (count, c)
              case _                                           => (count, prevCorner)
        wallCount._1 % 2 == 1

  def findLoopFixTiles(tiles: Map[Point, Tile]) =
    val start = tiles.values.find(t => t.symbol == 'S').get
    val neighbours = Seq(North, East, South, West)
      .flatMap(d => tiles.get(start.pos + d))
      .filter(_.neighbours.contains(start.pos))
    val neighbourPoints = neighbours.map(_.pos).toSet
    val fixedStart = "|-LJ7F"
      .map(Tile(_, start.pos))
      .find(t => t.neighbours == neighbourPoints)
      .get
    val fixedTiles = tiles ++ Map(fixedStart.pos -> fixedStart)

    val loop = mutable.ArrayBuffer[Tile](fixedStart)
    var current = neighbours.head
    while (current != fixedStart) do
      val next = fixedTiles(current.neighbours.find(_ != loop.last.pos).get)
      loop += current
      current = next

    (loop.toSeq, fixedTiles)

  def parseTiles(input: String) =
    input.linesIterator.zipWithIndex
      .flatMap((l, y) => l.zipWithIndex.map((t, x) => (t, x, y)))
      .map((t, x, y) => Point(x, y) -> Tile(t, Point(x, y)))
      .toMap

  case class Tile(symbol: Char, pos: Point):
    val neighbours: Set[Point] =
      symbol match
        case '|' => Set(pos + North, pos + South)
        case '-' => Set(pos + East, pos + West)
        case 'L' => Set(pos + North, pos + East)
        case 'J' => Set(pos + North, pos + West)
        case '7' => Set(pos + South, pos + West)
        case 'F' => Set(pos + South, pos + East)
        case _   => Set.empty

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)

  val North = Point(0, -1)
  val South = Point(0, 1)
  val West = Point(-1, 0)
  val East = Point(1, 0)
