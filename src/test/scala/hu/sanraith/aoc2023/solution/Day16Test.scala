package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/16 */
class Day16Test extends SolutionTestSpec:

  describe("Day16 for example input") {
    given day: Solution = Day16()

    it("solves part 1"):
      assertPart(day.part1, expected = 46, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 51, input = example)
  }

  describe("Day16 for puzzle input") {
    given day: Solution = Day16()

    it("solves part 1")(assertPart(day.part1, 7236))
    it("solves part 2")(assertPart(day.part2, 7521))
  }

  val example = """
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."""
