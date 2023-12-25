package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/24 */
class Day24Test extends SolutionTestSpec:

  describe("Day24 for example input") {
    given day: Solution = Day24()

    day.asInstanceOf[Day24].part1XYRange = 7L to 27L
    it("solves part 1"):
      assertPart(day.part1, expected = 2, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 47, input = example)
  }

  describe("Day24 for puzzle input") {
    given day: Solution = Day24()

    it("solves part 1")(assertPart(day.part1, 21785))
    it("solves part 2")(assertPart(day.part2, 554668916217145L))
  }

  val example = """
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3"""
