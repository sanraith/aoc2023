package hu.sanraith.aoc2023.solution

import scala.collection.mutable

/** Solution for https://adventofcode.com/2023/day/9 */
class Day09 extends Solution:
  override val title: String = "Mirage Maintenance"

  override def part1(ctx: Context): Int =
    ctx.input.linesIterator
      .map(_.split(" ").map(_.toInt))
      .map(history => getDiffs(history).tails.sum)
      .sum

  override def part2(ctx: Context): Int =
    ctx.input.linesIterator
      .map(_.split(" ").map(_.toInt))
      .map(history => getDiffs(history).heads.foldLeft(0)((acc, x) => x - acc))
      .sum

  def getDiffs(history: Seq[Int]) =
    val heads = mutable.Stack[Int]()
    val tails = mutable.Stack[Int]()
    var diff = history.toSeq
    while (diff.exists(_ != 0))
      heads.push(diff.head)
      tails.push(diff.last)
      diff = diff.sliding(2).map { case Seq(a, b) => b - a }.toSeq
    Diffs(heads, tails)

  case class Diffs(heads: mutable.Stack[Int], tails: mutable.Stack[Int])
