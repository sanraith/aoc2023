package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/__YEAR__/day/__DAY__ */
class Day__DAY_STR__Test extends SolutionTestSpec:

  describe("Day__DAY_STR__ for example input") {
    given day: Solution = Day__DAY_STR__()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = "__PART_1_TEST_EXPECTED__",
        input = """__PART_1_TEST_INPUT__"""
      )

    it("solves part 2"):
      _assertPart(
        day.part2,
        expected = "__PART_2_TEST_EXPECTED__",
        input = """__PART_2_TEST_INPUT__"""
      )
  }

  describe("Day__DAY_STR__ for puzzle input") {
    given day: Solution = Day__DAY_STR__()

    it("solves part 1")(_assertPart(day.part1, "__PART_1_EXPECTED__"))
    it("solves part 2")(_assertPart(day.part2, "__PART_2_EXPECTED__"))
  }
