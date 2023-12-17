package hu.sanraith.aoc2023.solution

import scala.collection.{mutable => mut}
import scala.io.StdIn.readLine
import scala.collection.SortedSet
import scala.collection.immutable.ListSet

/** Solution for https://adventofcode.com/2023/day/17 */
class Day17 extends Solution:
  val enableDebugPrint = false

  override val title: String = "Clumsy Crucible"

  override def part1(ctx: Context): Int =
    findMinimumLoss(
      ctx,
      selectDirections = (current, backwards, straights) =>
        Directions.filter(d => d != backwards && (d != current || straights < 3))
    )

  override def part2(ctx: Context): Int =
    findMinimumLoss(
      ctx,
      endCondition = straights => straights >= 4,
      selectDirections = (current, backwards, straights) =>
        straights match
          case c if c > 0 && c < 4 => Seq(current)
          case c if c < 10         => Directions.filter(d => d != backwards)
          case c if c == 10        => Directions.filter(d => d != backwards && d != current)
    )

  def findMinimumLoss(
      ctx: Context,
      endCondition: (straights: Int) => Boolean = _ => true,
      selectDirections: (
          direction: Direction,
          backwards: Direction,
          straights: Int
      ) => Seq[Direction]
  ) =
    val layout = parseLayout(ctx.input)
    val start = Point(0, 0)
    val end = Point(layout.width - 1, layout.height - 1)
    val maxDist = start.manhattan(end).toDouble
    var minDist = maxDist

    var keepSearching = true
    var optimalLoss = Int.MaxValue
    var optimalPath: Vector[(Point, Direction)] = Vector.empty
    val queue = mut.PriorityQueue(Step(start, NoDir, 0, 0, Vector((start, NoDir))))
    val visited: mut.Map[(Point, Direction, Int), Int] =
      mut.Map.empty // (pos, dir, straights) -> loss

    while (keepSearching && queue.size > 0)
      val Step(pos, dir, straights, loss, path) = queue.dequeue()
      minDist = math.min(pos.manhattan(end), minDist)
      ctx.progress((maxDist - minDist) / maxDist)

      if (loss > optimalLoss) // We are only looking at worse paths from now
        keepSearching = false
      else if (pos == end && endCondition(straights)) // Found a possible solution
        if (loss < optimalLoss)
          optimalLoss = loss
          optimalPath = path
      else if (!visited.get((pos, dir, straights)).exists(_ <= loss))
        // Visit pos if the direction and straight count is new or current loss is better
        visited.addOne((pos, dir, straights), loss)
        val backwards = Backwards.get(dir).getOrElse(dir)
        val nextDirs = selectDirections(dir, backwards, straights)
        for (nextDir <- nextDirs.filter(d => layout.tiles.contains(pos + d)))
          val nextPos = pos + nextDir
          val nextLoss = loss + layout.tiles(nextPos)
          val nextStraights = if (nextDir == dir) straights + 1 else 1
          val nextPath = if (enableDebugPrint) path.appended(nextPos -> nextDir) else path
          queue.enqueue(Step(nextPos, nextDir, nextStraights, nextLoss, nextPath))
    end while

    debugPrint(layout, optimalPath)
    optimalLoss
  end findMinimumLoss

  def debugPrint(layout: Layout, path: Vector[(Point, Direction)]): Unit =
    if (!enableDebugPrint) return
    val heatMap = Seq('-', '█', '█', '▓', '▓', '▒', '▒', '░', '░', ' ')
    val pathMap = path.toMap
    for (y <- 0 until layout.height)
      val line = (0 until layout.width).map: x =>
        val p = Point(x, y)
        pathMap.get(p) match
          case Some(direction) if Backwards.contains(direction) =>
            direction match
              case East  => '>'
              case South => 'v'
              case West  => '<'
              case North => '^'
          case _ => heatMap(layout.tiles(p))
      println(line.mkString)
    println("")
  end debugPrint

  def parseLayout(input: String) =
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    val tiles = input.linesIterator.zipWithIndex
      .flatMap((l, y) => l.split("").zipWithIndex.map((n, x) => Point(x, y) -> n.toInt))
      .toMap
    Layout(tiles, width, height)

  implicit val stepOrdering: Ordering[Step] = Ordering.by(s => -s.loss)
  case class Step(
      pos: Point,
      dir: Direction,
      straights: Int,
      loss: Int,
      path: Vector[(Point, Direction)]
  )

  case class Layout(tiles: Map[Point, Int], width: Int, height: Int)

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs

  type Direction = Point
  val East: Direction = Point(1, 0)
  val South: Direction = Point(0, 1)
  val West: Direction = Point(-1, 0)
  val North: Direction = Point(0, -1)
  val NoDir: Direction = Point(0, 0)
  val Directions = Seq(East, South, West, North)
  val Backwards = Directions.zipWithIndex.map((d, i) => d -> Directions((i + 2) % 4)).toMap

end Day17
