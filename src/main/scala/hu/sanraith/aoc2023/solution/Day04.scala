package hu.sanraith.aoc2023.solution

import scala.collection.mutable

/** Solution for https://adventofcode.com/2023/day/4 */
class Day04 extends Solution:
  override val title: String = "Scratchcards"

  override def part1(ctx: Context): Long =
    parseCards(ctx.input).map(card => Math.pow(2, card.matchCount - 1).toInt).sum

  override def part2(ctx: Context): Long =
    given countCache: mutable.Map[Int, Long] = mutable.Map() // id -> count
    val cards = parseCards(ctx.input)
    val cardMap = cards.map(c => c.id -> c).toMap
    cards.map(c => scratchAndCount(c.id, cardMap)).sum

  def scratchAndCount(id: Int, cardMap: Map[Int, Card])(using cache: mutable.Map[Int, Long]): Long =
    cache.getOrElseUpdate(
      id,
      cardMap
        .get(id)
        .map(c => 1 + (id + 1 to id + c.matchCount).map(scratchAndCount(_, cardMap)).sum)
        .getOrElse(0)
    )

  def parseCards(input: String) =
    val lineRegex = """(\d+): (.*) \| (.*)""".r
    val numberRegex = """\d+""".r
    input.linesIterator
      .flatMap(lineRegex.findFirstMatchIn)
      .map: parts =>
        val id = parts.group(1).toInt
        val winningNums = numberRegex.findAllIn(parts.group(2)).map(_.toInt).toSeq
        val myNums = numberRegex.findAllIn(parts.group(3)).map(_.toInt).toSeq
        val matchCount = myNums.intersect(winningNums).length
        Card(id, matchCount)
      .toList

  case class Card(id: Int, matchCount: Int)
