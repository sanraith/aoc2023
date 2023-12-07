package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/7 */
class Day07Test extends SolutionTestSpec:

  describe("Day07 for example input") {
    given day: Solution = Day07()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = "6440",
        input = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""
      )

    it("solves part 2"):
      assertPart(
        day.part2,
        expected = "5905",
        input = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""
      )
  }

  describe("Day07 for puzzle input") {
    given day: Solution = Day07()

    it("solves part 1")(assertPart(day.part1, "248559379"))
    it("solves part 2")(assertPart(day.part2, "249631254"))
  }
