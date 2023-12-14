package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/14 */
class Day14Test extends SolutionTestSpec:

  describe("Day14 for example input") {
    given day: Solution = Day14()

    it("solves part 1"):
      assertPart(day.part1, expected = 136, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 64, input = example)
  }

  describe("Day14 for puzzle input") {
    given day: Solution = Day14()

    it("solves part 1")(assertPart(day.part1, 106378))
    it("solves part 2")(assertPart(day.part2, 90795))
  }

  val example = """
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""
