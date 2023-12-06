package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/6 */
class Day06Test extends SolutionTestSpec:

  describe("Day06 for example input") {
    given day: Solution = Day06()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = "288",
        input = """
Time:      7  15   30
Distance:  9  40  200"""
      )

    it("solves part 2"):
      assertPart(
        day.part2,
        expected = "71503",
        input = """
Time:      7  15   30
Distance:  9  40  200"""
      )
  }

  describe("Day06 for puzzle input") {
    given day: Solution = Day06()

    it("solves part 1")(assertPart(day.part1, "3317888"))
    it("solves part 2")(assertPart(day.part2, "24655068"))
  }
