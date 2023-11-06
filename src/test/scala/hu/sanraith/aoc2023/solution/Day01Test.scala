package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

class Day01Test extends SolutionTestSpec {

  describe("Day01 for test input") {
    given day: Solution = Day01()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = """aaaaaa
bbbbbb
cccccc""",
        input = """example input"""
      )

    it("solves part 2"):
      _assertPart(
        day.part2,
        expected = "EXPECTED_2",
        input = """TEST_INPUT_2"""
      )
  }

  describe("Day01 for puzzle input") {
    given day: Solution = Day01()

    it("solves part 1")(_assertPart(day.part1, "EXPECTED_1"))

    it("solves part 2")(_assertPart(day.part2, "EXPECTED_2"))
  }

}
