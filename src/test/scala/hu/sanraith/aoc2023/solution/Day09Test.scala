package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/9 */
class Day09Test extends SolutionTestSpec:

  describe("Day09 for example input") {
    given day: Solution = Day09()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = 114,
        input = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""
      )

    it("solves part 2"):
      assertPart(
        day.part2,
        expected = 2,
        input = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""
      )
  }

  describe("Day09 for puzzle input") {
    given day: Solution = Day09()

    it("solves part 1")(assertPart(day.part1, 2043677056))
    it("solves part 2")(assertPart(day.part2, "1062"))
  }
