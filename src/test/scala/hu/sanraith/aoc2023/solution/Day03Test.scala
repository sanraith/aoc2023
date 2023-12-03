package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/3 */
class Day03Test extends SolutionTestSpec:

  describe("Day03 for example input") {
    given day: Solution = Day03()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = "4361",
        input = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
      )

    it("solves part 2"):
      assertPart(
        day.part2,
        expected = "467835",
        input = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
      )
  }

  describe("Day03 for puzzle input") {
    given day: Solution = Day03()

    it("solves part 1")(assertPart(day.part1, "551094"))
    it("solves part 2")(assertPart(day.part2, "80179647"))
  }
