package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/18 */
class Day18Test extends SolutionTestSpec:

  describe("Day18 for example input") {
    given day: Solution = Day18()

    it("solves part 1"):
      assertPart(day.part1, expected = 62, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 952408144115L, input = example)
  }

  describe("Day18 for puzzle input") {
    given day: Solution = Day18()

    it("solves part 1")(assertPart(day.part1, 56678))
    it("solves part 2")(assertPart(day.part2, 79088855654037L))
  }

  val example = """
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"""
