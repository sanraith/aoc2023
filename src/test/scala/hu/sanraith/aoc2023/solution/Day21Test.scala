package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/21 */
class Day21Test extends SolutionTestSpec:

  describe("Day21 for example input") {
    given day: Solution = Day21()

    it("solves part 1"):
      day.asInstanceOf[Day21].part1MaxSteps = 6
      assertPart(
        day.part1,
        expected = 16,
        input = """
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."""
      )

    it("solves part 2"):
      _assertPart(
        day.part2,
        expected = "__PART_2_TEST_EXPECTED__",
        input = """__PART_2_TEST_INPUT__"""
      )
  }

  describe("Day21 for puzzle input") {
    given day: Solution = Day21()

    it("solves part 1")(assertPart(day.part1, 3788))
    it("solves part 2")(_assertPart(day.part2, "__PART_2_EXPECTED__"))
  }
