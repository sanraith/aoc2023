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

  /** Combine ranges on 2 levels by
    *   - creating distinct ranges of a.dest & b.src
    *   - combining their delta where they overlap
    *   - normalizing them to level 1
    * TODO refactor I guess
    */
  def combineRanges(rda: Seq[RangeDelta], rdb: Seq[RangeDelta]) =
    val rdai = rda.zipWithIndex
    val rdbi = rdb.zipWithIndex
    (rda.flatMap(rd => List(rd.source.start + rd.delta, rd.source.end + rd.delta)) ++ rdb.flatMap(
      rd => List(rd.source.start, rd.source.end)
    )).toSet.toSeq.sorted
      .sliding(2)
      .toList
      .map { case Seq(x1, x2) =>
        (
          x1,
          x2,
          rdai.map((rd, i) => x1 >= rd.source.start + rd.delta && x2 <= rd.source.end + rd.delta),
          rdbi.map((rd, i) => x1 >= rd.source.start && x2 <= rd.source.end)
        )
      }
      .groupBy { (x1, x2, ma, mb) => ma.count(x => x) + mb.count(x => x) }
      .toList
      .flatMap((k, s) =>
        k match
          case 0 => Seq.empty
          case _ =>
            s.map { case (start, end, ma, mb) =>
              val deltaA = ma.zipWithIndex.find((x, _) => x).map((_, i) => rdai(i)._1.delta)
              val deltaB = mb.zipWithIndex.find((x, _) => x).map((_, i) => rdbi(i)._1.delta)
              (deltaA, deltaB) match
                case (None, None)         => throw new Exception("logic error")
                case (Some(da), None)     => RangeDelta(start - da until end - da, da)
                case (None, Some(db))     => RangeDelta(start until end, db)
                case (Some(da), Some(db)) => RangeDelta(start - da until end - da, da + db)
            }
      )

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
