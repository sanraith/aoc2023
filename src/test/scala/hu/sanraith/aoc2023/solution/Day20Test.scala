package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/20 */
class Day20Test extends SolutionTestSpec:

  describe("Day20 for example input") {
    given day: Solution = Day20()

    it("solves part 1 for example 1"):
      assertPart(
        day.part1,
        expected = 32000000,
        input = """
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a"""
      )

    it("solves part 1 for example 2"):
      assertPart(
        day.part1,
        expected = 11687500,
        input = """
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output"""
      )

    it("solves part 2"):
      _assertPart(
        day.part2,
        expected = "__PART_2_TEST_EXPECTED__",
        input = """__PART_2_TEST_INPUT__"""
      )
  }

  describe("Day20 for puzzle input") {
    given day: Solution = Day20()

    it("solves part 1")(assertPart(day.part1, 925955316))
    it("solves part 2")(_assertPart(day.part2, "__PART_2_EXPECTED__"))
  }
