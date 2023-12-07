package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/7 */
class Day07 extends Solution:
  override val title: String = "Camel Cards"

  override def part1(ctx: Context): Int =
    parseHands(ctx.input, useJoker = false).sorted.zipWithIndex
      .map((h, i) => h.bid * (i + 1))
      .sum

  override def part2(ctx: Context): Int =
    parseHands(ctx.input, useJoker = true).sorted.zipWithIndex
      .map((h, i) => h.bid * (i + 1))
      .sum

  val CardValuesWithoutJoker = "AKQJT98765432".reverse.zipWithIndex.toMap
  val CardValuesWithJoker = "AKQT98765432J".reverse.zipWithIndex.toMap
  val CardTypeCount = CardValuesWithJoker.size

  def parseHands(input: String, useJoker: Boolean) =
    val cardValues = if (useJoker) CardValuesWithJoker else CardValuesWithoutJoker
    val jValues = if (useJoker) "AKQT98765432" else "J"
    input.linesIterator
      .map: line =>
        val Array(cards, bidStr) = line.split(" ")
        Hand(cards, bidStr.toInt, cardValues, jValues)
      .toSeq

  implicit val handOrdering: Ordering[Hand] = Ordering.by(h => (h.handType, h.highCardValue))
  case class Hand(
      val cards: Iterable[Char],
      val bid: Int,
      cardValues: Map[Char, Int],
      jValues: Iterable[Char]
  ):
    val highCardValue: Long = cards.foldLeft(0L)((acc, c) => acc * CardTypeCount + cardValues(c))
    val handType: Int =
      jValues
        .map(j => cards.map(card => if (card == 'J') j else card))
        .map: cards =>
          val cardCounts = cards.groupBy(c => c).map((c, s) => s.size).toSeq.sorted.reverse
          cardCounts.head match
            case 5                       => 6 // Five of a kind
            case 4                       => 5 // Four of a kind
            case 3 if cardCounts(1) == 2 => 4 // Full house
            case 3                       => 3 // Three of a kind
            case 2 if cardCounts(1) == 2 => 2 // Two pairs
            case 2                       => 1 // One pair
            case 1                       => 0 // High card
        .max

end Day07
