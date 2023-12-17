package hu.sanraith.aoc2023.solution

import scala.collection.mutable

/** Solution for https://adventofcode.com/2023/day/14 */
class Day14 extends Solution:
  override val title: String = "Parabolic Reflector Dish"

  override def part1(ctx: Context): Long =
    parseMap(ctx.input).tiltNorth.calcLoad

  override def part2(ctx: Context): Long =
    var map = parseMap(ctx.input)
    val states = mutable.Map[String, (Int, Int)]() // state -> (load, cycleStart)
    val target = 1000000000
    var it = 0
    while (it < target)
      ctx.progress(it.toDouble / 130) // heuristic
      val hash = map.calcHash
      states.get(hash) match
        case Some(load, cycleStart) =>
          val cycleSize = it - cycleStart
          val remaining = target - it
          it += (remaining / cycleSize) * cycleSize
        case None => states(hash) = (map.calcLoad, it)
      map = (0 until 4).foldLeft(map)((map, _) => map.tiltNorth.rotateClockwise)
      it += 1
    map.calcLoad

  def parseMap(input: String) =
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    val content = input.linesIterator.zipWithIndex
      .flatMap((l, y) => l.zipWithIndex.filter((c, _) => c != '.') map ((c, x) => Point(x, y) -> c))
      .toMap
    RockMap(content, width, height)

  case class Point(x: Int, y: Int):
    override def toString: String = s"($x,$y)"

  case class RockMap(content: Map[Point, Char], width: Int, height: Int):
    def calcLoad =
      content.filter((_, c) => c == 'O').map((p, _) => height - p.y).sum

    def calcHash =
      content.filter((_, c) => c == 'O').map((p, _) => p).toSeq.sortBy(p => (p.y, p.x)).mkString

    def tiltNorth =
      val columns = content
        .groupBy((p, _) => p.x)
        .map((x, s) => x -> mutable.Seq.from(s).sortBy((p, _) => p.y))
      for ((x, column) <- columns)
        val spheres = column.zipWithIndex.filter { case ((_, c), _) => c == 'O' }
        for (((p -> c), i) <- spheres)
          val y = column.lift(i - 1).map((p, _) => p.y + 1).getOrElse(0)
          column(i) = Point(x, y) -> c
      RockMap(columns.values.flatten.toMap, width, height)

    def rotateClockwise = RockMap(
      content.map((p, c) => Point(height - p.y - 1, p.x) -> c),
      width = height,
      height = width
    )

    def debugPrint =
      for (y <- 0 until height)
        val line = (0 until width).map(x => content.get(Point(x, y)).getOrElse('.')).mkString
        println(line)

  end RockMap
