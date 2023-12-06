package hu.sanraith.aoc2023.solution

import scala.collection.immutable.NumericRange.{Exclusive => RangeEx}

/** Solution for https://adventofcode.com/2023/day/5 */
class Day05 extends Solution:
  override val title: String = "If You Give A Seed A Fertilizer"

  override def part1(ctx: Context): String =
    val (seeds, levels) = parseMaps(ctx.input)
    seeds
      .map: seed =>
        levels.foldLeft(seed): (index, rangeMaps) =>
          rangeMaps
            .find(_.source.contains(index))
            .map(range => index + range.delta)
            .getOrElse(index)
      .min
      .toString

  override def part2(ctx: Context): String =
    val (seeds, levels) = parseMaps(ctx.input)
    val seedRanges = seeds
      .grouped(2)
      .map { case Seq(start, length) => RangeMap(start until start + length, delta = 0) }
      .toSeq
    levels
      .foldLeft(seedRanges)((acc, rangeMaps) => combineRangeMaps(acc, rangeMaps))
      .filter(r => seedRanges.exists(_.source.intersects(r.source)))
      .map(_.dest.start)
      .min
      .toString

  /** Combine range maps on two levels
    *   - creating distinct ranges of a.dest & b.source
    *   - combining their delta where they overlap
    *   - normalizing them to level 1
    */
  def combineRangeMaps(rangesA: Seq[RangeMap], rangesB: Seq[RangeMap]) =
    val aPoints = rangesA.flatMap(r => List(r.dest.start, r.dest.end))
    val bPoints = rangesB.flatMap(r => List(r.source.start, r.source.end))
    (aPoints ++ bPoints).toSet.toSeq.sorted
      .sliding(2)
      .toList
      .flatMap { case Seq(start, end) =>
        val deltaA = rangesA.find(_.dest.intersects(start until end)).map(_.delta)
        val deltaB = rangesB.find(_.source.intersects(start until end)).map(_.delta)
        (deltaA, deltaB) match
          case (None, None) => None
          case (_, _) =>
            val (dA, dB) = (deltaA.getOrElse(0L), deltaB.getOrElse(0L))
            Some(RangeMap(start - dA until end - dA, dA + dB))
      }

  def parseMaps(input: String) =
    val numberRegex = """\d+""".r
    val seedRegex = """(?<=seeds: ).*""".r
    val mapRegex = """(?ms)(\w+)-to-(\w+) map:([^A-Za-z]+)""".r
    val seeds = seedRegex.findFirstIn(input).flatMap(numberRegex.findAllIn).map(_.toLong).toSeq
    val levels = mapRegex
      .findAllMatchIn(input)
      .map { case mapRegex(srcStr, destStr, rangesStr) =>
        rangesStr.trim.linesIterator
          .map(numberRegex.findAllIn(_).map(_.toLong).toSeq)
          .map { case Seq(dest, src, length) => RangeMap(src until src + length, dest - src) }
          .toSeq
      }
      .toSeq
    (seeds, levels)

  case class RangeMap(source: RangeEx[Long], delta: Long):
    val dest: RangeEx[Long] = source.start + delta until source.end + delta

  implicit class RangeOps(r1: RangeEx[Long]):
    def intersects(r2: RangeEx[Long]): Boolean =
      r1.contains(r2.start) || r1.contains(r2.end - 1)

end Day05
