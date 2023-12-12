package hu.sanraith.aoc2023.solution

import hu.sanraith.aoc2023.util._

import scala.collection.StringView
import scala.collection.mutable

/** Solution for https://adventofcode.com/2023/day/12 */
class Day12 extends Solution:
  override val title: String = "Hot Springs"

  override def part1(ctx: Context): Long =
    parseInput(ctx.input)
      .map(countArrangements)
      .sum

  override def part2(ctx: Context): Long =
    val rowCount = ctx.input.linesIterator.length.toDouble
    parseInput(ctx.input)
      .map: (records, brkGroups) =>
        (
          (0 until 5).map(_ => records).mkString("?"),
          (0 until 5).flatMap(_ => brkGroups)
        )
      .tapEachWithIndex((_, i) => ctx.progress(i / rowCount))
      .map(countArrangements)
      .sum

  def countArrangements(records: String, brkGroups: Seq[Int]) =
    val opCount = records.length - brkGroups.sum
    val opRanges = (0 to brkGroups.length).map: id =>
      if (id == 0 || id == brkGroups.length) (0 to opCount)
      else (1 to opCount)

    given arrangementCache: mutable.Map[(Int, Int, Int), Long] = mutable.Map()
    _countArrangements(0, 0, opCount, records.view, opRanges.toVector, brkGroups.toVector)

  def _countArrangements(
      pos: Int,
      opGroupIndex: Int,
      remainingSpaces: Int,
      records: StringView,
      opRanges: Vector[Range],
      brokenGroups: Vector[Int]
  )(using cache: mutable.Map[(Int, Int, Int), Long]): Long =
    cache.getOrElseUpdate(
      (pos, opGroupIndex, remainingSpaces), {
        val range = opRanges(opGroupIndex).start to
          Math.min(
            opRanges(opGroupIndex).end,
            remainingSpaces - (opRanges.length - opGroupIndex - 2)
          )
        val nextBrokenGroupSize = brokenGroups.lift(opGroupIndex).getOrElse(0)
        range
          .takeWhile(opSize => isValidGroup(records, pos, opSize, operational = true))
          .filter: opSize =>
            brokenGroups
              .lift(opGroupIndex)
              .map(brkSize => isValidGroup(records, pos + opSize, brkSize, operational = false))
              .getOrElse(opSize == remainingSpaces)
          .map: opSize =>
            // println(s"${"-" * opGroupIndex} @$pos op: $opSize from $range")
            if (opGroupIndex + 1 == opRanges.length) 1
            else
              _countArrangements(
                pos + opSize + nextBrokenGroupSize,
                opGroupIndex + 1,
                remainingSpaces - opSize,
                records,
                opRanges,
                brokenGroups
              )
          .sum
      }
    )

  def isValidGroup(records: StringView, start: Int, length: Int, operational: Boolean): Boolean =
    if (operational) records.slice(start, start + length).forall(c => c == '.' || c == '?')
    else records.slice(start, start + length).forall(c => c == '#' || c == '?')

  def parseInput(input: String) =
    input.linesIterator
      .map(_.split(" ").toSeq)
      .map { case Seq(records, brkGroups) => (records, brkGroups.split(",").map(_.toInt).toSeq) }
