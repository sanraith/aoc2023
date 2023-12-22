package hu.sanraith.aoc2023.solution

import hu.sanraith.aoc2023.util._
import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/22 */
class Day22 extends Solution:
  override val title: String = "Sand Slabs"

  override def part1(ctx: Context): Int =
    val bricks = dropToFloor(parseBricks(ctx.input))
    val (_, supportedBy) = createSupportMaps(bricks)
    val unsafeSupports = supportedBy
      .filter((_, v) => v.size == 1)
      .flatMap((_, v) => v)
      .toSet
    bricks.size - unsafeSupports.size

  override def part2(ctx: Context): Int =
    val bricks = dropToFloor(parseBricks(ctx.input))
    val (supports, supportedBy) = createSupportMaps(bricks)
    supports
      .filter((_, v) => v.nonEmpty)
      .map((b, _) => countFallen(Set(b), mut.Set(b), supports, supportedBy))
      .sum

  def countFallen(
      bricksToRemove: Set[Brick],
      removedBricks: mut.Set[Brick],
      supports: Map[Brick, Set[Brick]],
      supportedBy: Map[Brick, Set[Brick]]
  ): Int =
    val nextBricks = bricksToRemove.flatMap: brickToRemove =>
      supports
        .get(brickToRemove)
        .getOrElse(Seq.empty)
        .filter(r => supportedBy(r).subsetOf(removedBricks))
        .toSet
    removedBricks.addAll(nextBricks)
    if (nextBricks.isEmpty) 0
    else nextBricks.size + countFallen(nextBricks, removedBricks, supports, supportedBy)

  // (support -> supported, supported -> support)
  def createSupportMaps(bricks: Seq[Brick]) =
    val supportsMap = bricks.zipWithIndex
      .flatMap: (brick, i) =>
        if (brick.rangeZ.start == 1) Seq.empty
        else
          val below = bricks.view.slice(i + 1, bricks.size)
          val supportLevel = brick.rangeZ.start - 1
          below
            .filter: r =>
              r.rangeZ.end == supportLevel &&
                r.rangeX.intersects(brick.rangeX) &&
                r.rangeY.intersects(brick.rangeY)
            .map(support => support -> brick)
            .toSeq
      .groupBy((support, _) => support)
      .map((support, bricks) => support -> bricks.map((_, brick) => brick).toSet)
      .toMap
    val supportedByMap =
      bricks.map(b => b -> supportsMap.filter((_, v) => v.contains(b)).map((k, _) => k).toSet).toMap
    (supportsMap, supportedByMap)

  def dropToFloor(bricks_ : Seq[Brick]) =
    val bricks = bricks_.sortBy(_.rangeZ.start)
    var bricksOnFloor = mut.ArrayBuffer.empty[Brick]
    bricks.foreach: b =>
      bricksOnFloor.find(r => r.rangeX.intersects(b.rangeX) && r.rangeY.intersects(b.rangeY)) match
        case Some(r) => bricksOnFloor += b - Point(0, 0, b.rangeZ.start - r.rangeZ.end - 1)
        case None    => bricksOnFloor += b - Point(0, 0, b.rangeZ.start - 1)
      bricksOnFloor.sortInPlace()(Ordering.by(b => -b.rangeZ.end))
    bricksOnFloor.toSeq

  def parseBricks(input: String) =
    input.linesIterator
      .map: line =>
        val points = line
          .split("~")
          .map(_.split(",").map(_.toInt))
          .map { case Array(x, y, z) => Point(x, y, z) }
          .sortBy(p => (p.z, p.y, p.x))
        Brick(points(0), points(1))
      .toSeq

  case class Brick(a: Point, b: Point):
    val rangeX = a.x to b.x
    val rangeY = a.y to b.y
    val rangeZ = a.z to b.z
    def +(delta: Point): Brick = Brick(a + delta, b + delta)
    def -(delta: Point): Brick = Brick(a - delta, b - delta)
    override def toString: String = s"$a~$b"

  case class Point(x: Int, y: Int, z: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
    def -(other: Point): Point = Point(x - other.x, y - other.y, z - other.z)
    def unary_- : Point = Point(-x, -y, -z)
    override def toString: String = s"($x,$y,$z)"

  implicit class RangeOps(r1: Range.Inclusive):
    def intersects(r2: Range.Inclusive): Boolean =
      r1.start <= r2.end && r2.start <= r1.end
