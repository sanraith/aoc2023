package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/13 */
class Day13Test extends SolutionTestSpec:

  describe("Day13 for example input") {
    given day: Solution = Day13()

    it("solves part 1"):
      assertPart(day.part1, expected = 405, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 400, input = example)
  }

  describe("Day13 for puzzle input") {
    given day: Solution = Day13()

    it("solves part 1")(assertPart(day.part1, 34993))
    it("solves part 2")(assertPart(day.part2, 29341))
  }

  val example = """
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""
