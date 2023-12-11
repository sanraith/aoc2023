package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/11 */
class Day11Test extends SolutionTestSpec:

  describe("Day11 for example input") {
    given day: Solution = Day11()

    it("solves part 1"):
      assertPart(day.part1, expected = 374, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 82000210, input = example)
  }

  describe("Day11 for custom input") {
    given day: Solution = Day11()

    it("solves part 2 for single distance"):
      assertPart(day.part2, expected = 1000001, input = "#.#")
  }

  describe("Day11 for puzzle input") {
    given day: Solution = Day11()

    it("solves part 1")(assertPart(day.part1, 9734203))
    it("solves part 2")(assertPart(day.part2, 568914596391L))
  }

  val example = """
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""
