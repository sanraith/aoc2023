package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/22 */
class Day22Test extends SolutionTestSpec:

  describe("Day22 for example input") {
    given day: Solution = Day22()

    it("solves part 1"):
      assertPart(day.part1, expected = 5, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 7, input = example)
  }

  describe("Day22 for puzzle input") {
    given day: Solution = Day22()

    it("solves part 1")(assertPart(day.part1, 430))
    it("solves part 2")(assertPart(day.part2, 60558))
  }

  val example = """
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9"""
