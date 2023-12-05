package hu.sanraith.aoc2023.solution

type LongRange = scala.collection.immutable.NumericRange.Exclusive[Long]

/** Solution for https://adventofcode.com/2023/day/5 */
class Day05 extends Solution:
  override val title: String = "If You Give A Seed A Fertilizer"

  override def part1(ctx: Context): String =
    val (seeds, maps) = parseMaps(ctx.input)
    seeds
      .map: seed =>
        maps.foldLeft(seed): (index, target) =>
          target.ranges
            .find(_.source.contains(index))
            .map(range => index + range.delta)
            .getOrElse(index)
      .min
      .toString

  override def part2(ctx: Context): String =
    val (seeds, maps) = parseMaps(ctx.input)
    val seedRanges = seeds
      .grouped(2)
      .map { case Seq(from, length) => RangeDelta(from until from + length, 0) }
      .toSeq
    maps
      .map(_.ranges)
      .foldLeft(seedRanges)((acc, x) => combineRanges(acc, x))
      .filter: x =>
        seedRanges.exists(s => x.source.start >= s.source.start && x.source.end <= s.source.end)
      .map(x => x.source.start + x.delta)
      .sorted
      .head
      .toString

  /** Combine ranges on level 1 & 2 by
    *   - creating distinct ranges of a.destination & b.source
    *   - adding their delta where they overlap
    *   - normalizing them to level 1
    */
  def combineRanges(rangesA: Seq[RangeDelta], rangesB: Seq[RangeDelta]) =
    val aPoints = rangesA.flatMap(r => List(r.source.start + r.delta, r.source.end + r.delta))
    val bPoints = rangesB.flatMap(r => List(r.source.start, r.source.end))
    (aPoints ++ bPoints).toSet.toSeq.sorted
      .sliding(2)
      .toList
      .flatMap { case Seq(from, to) =>
        val deltaA = rangesA
          .find(r => from >= r.source.start + r.delta && to <= r.source.end + r.delta)
          .map(_.delta)
        val deltaB = rangesB.find(r => from >= r.source.start && to <= r.source.end).map(_.delta)
        (deltaA, deltaB) match
          case (None, None)         => None
          case (Some(dA), None)     => Some(RangeDelta(from - dA until to - dA, dA))
          case (None, Some(dB))     => Some(RangeDelta(from until to, dB))
          case (Some(dA), Some(dB)) => Some(RangeDelta(from - dA until to - dA, dA + dB))
      }

  def parseMaps(input: String) =
    val numberRegex = """\d+""".r
    val seedRegex = """(?<=seeds: ).*""".r
    val mapRegex = """(?ms)(\w+)-to-(\w+) map:([^A-Za-z]+)""".r

    val seeds = seedRegex.findFirstIn(input).flatMap(numberRegex.findAllIn).map(_.toLong).toSeq
    val maps = mapRegex
      .findAllMatchIn(input)
      .map { case mapRegex(srcStr, destStr, rangesStr) =>
        val ranges = rangesStr.trim.linesIterator
          .map(numberRegex.findAllIn(_).map(_.toLong).toSeq)
          .map { case Seq(dest, src, length) => RangeDelta(src until src + length, dest - src) }
          .toSeq
        GardenMap(srcStr, destStr, ranges)
      }
      .toSeq
    (seeds, maps)

  case class GardenMap(source: String, dest: String, ranges: Seq[RangeDelta])
  case class RangeDelta(source: LongRange, delta: Long)
