package hu.sanraith.aoc2023.solution

/** Solution for https://adventofcode.com/2023/day/7 */
class Day07 extends Solution:
  override val title: String = "Camel Cards"

  override def part1(ctx: Context): String =
    parseHands(ctx.input, useJoker = false).sorted.zipWithIndex
      .map((h, i) => h.bid * (i + 1))
      .sum
      .toString

  override def part2(ctx: Context): String =
    parseHands(ctx.input, useJoker = true).sorted.zipWithIndex
      .map((h, i) => h.bid * (i + 1))
      .sum
      .toString

  val CardOrderWithoutJoker = "AKQJT98765432".reverse.zipWithIndex.toMap
  val CardOrderWithJoker = "AKQT98765432J".reverse.zipWithIndex.toMap
  val CardTypeCount = CardOrderWithJoker.size

  def parseHands(input: String, useJoker: Boolean) =
    val cardOrder = if (useJoker) CardOrderWithJoker else CardOrderWithoutJoker
    val jValues = if (useJoker) "AKQT98765432" else "J"
    input.linesIterator
      .map: line =>
        val Array(cards, bidStr) = line.split(" ")
        Hand(cards, bidStr.toInt, cardOrder, jValues)
      .toSeq

  implicit val handOrdering: Ordering[Hand] = Ordering.by(h => (h.handType, h.highCardValue))
  case class Hand(
      val cards: Iterable[Char],
      val bid: Int,
      cardOrder: Map[Char, Int],
      jValues: Iterable[Char]
  ):
    val highCardValue: Long = cards.foldLeft(0L)((acc, c) => acc * CardTypeCount + cardOrder(c))
    val handType: Int =
      jValues
        .map(newValue => cards.map(c => if (c == 'J') newValue else c))
        .map: cards =>
          val cardCounts = cards.groupBy(c => c).map((c, s) => s.size).toSeq.sorted.reverse
          cardCounts.head match
            case 5                       => 6 // Five of a kind
            case 4                       => 5 // Four of a kind
            case 3 if cardCounts(1) == 2 => 4 // Full house
            case 3                       => 3 // Three of a kind
            case 2 if cardCounts(1) == 2 => 2 // Two pair
            case 2                       => 1 // One pair
            case 1                       => 0 // High card
        .max

end Day07
