package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/15 */
class Day15Test extends SolutionTestSpec:

  describe("Day15 for example input") {
    given day: Solution = Day15()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = 1320,
        input = """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""
      )

    it("solves part 2"):
      assertPart(
        day.part2,
        expected = 145,
        input = """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""
      )
  }

  describe("Day15 for puzzle input") {
    given day: Solution = Day15()

    it("solves part 1")(assertPart(day.part1, 515495))
    it("solves part 2")(assertPart(day.part2, 229349))
  }
