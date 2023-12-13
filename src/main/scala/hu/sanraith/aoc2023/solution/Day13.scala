package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/13 */
class Day13 extends Solution:
  override val title: String = "Point of Incidence"

  override def part1(ctx: Context): Int =
    val maps = parseMaps(ctx.input)
    val horizontalCount = maps.map(countHorizontalMirrored(_)).sum
    val verticalCount = maps.map(rotateClockwise).map(countHorizontalMirrored(_)).sum
    horizontalCount * 100 + verticalCount

  override def part2(ctx: Context): Int =
    val maps = parseMaps(ctx.input)
    val horizontalCount = maps.map(countHorizontalMirrored(_, smudges = 1)).sum
    val verticalCount = maps.map(rotateClockwise).map(countHorizontalMirrored(_, smudges = 1)).sum
    horizontalCount * 100 + verticalCount

  def rotateClockwise(map: MirrorMap) = MirrorMap(
    map.content.map((p, c) => Point(map.height - p.y - 1, p.x) -> c),
    width = map.height,
    height = map.width
  )

  def countHorizontalMirrored(map: MirrorMap, smudges: Int = 0) =
    val patterns = map.content
    (0 until map.height - 1)
      .map: mirrorTop =>
        var top = mirrorTop
        var bottom = top + 1
        var remainingSmudges = smudges
        while (remainingSmudges >= 0 && top >= 0 && bottom < map.height) do
          val errorCount = (0 until map.width)
            .count(x => patterns(Point(x, top)) != patterns(Point(x, bottom)))
          remainingSmudges -= errorCount
          top -= 1
          bottom += 1
        if (remainingSmudges == 0) mirrorTop + 1 else 0
      .max

  def parseMaps(input: String) =
    val mapRegex = """(?s)\S.*?(?=\R{2}|\s*$)""".r
    mapRegex
      .findAllIn(input)
      .map: mapStr =>
        val width = mapStr.linesIterator.next.length
        val height = mapStr.linesIterator.length
        val points = mapStr.linesIterator.zipWithIndex
          .flatMap((l, y) => l.zipWithIndex.map((c, x) => Point(x, y) -> c))
          .toMap
        MirrorMap(points, width, height)
      .toVector

  case class MirrorMap(content: Map[Point, Char], width: Int, height: Int)
  case class Point(x: Int, y: Int)
