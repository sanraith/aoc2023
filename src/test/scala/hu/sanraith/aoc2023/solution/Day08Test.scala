package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/8 */
class Day08Test extends SolutionTestSpec:

  describe("Day08 for example input") {
    given day: Solution = Day08()

    it("solves part 1 for example 1"):
      assertPart(
        day.part1,
        expected = 2,
        input = """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""
      )

    it("solves part 1 for example 2"):
      assertPart(
        day.part1,
        expected = 6,
        input = """
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""
      )

    it("solves part 2"):
      assertPart(
        day.part2,
        expected = 6,
        input = """
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""
      )
  }

  describe("Day08 for puzzle input") {
    given day: Solution = Day08()

    it("solves part 1")(assertPart(day.part1, 14257))
    it("solves part 2")(assertPart(day.part2, 16187743689077L))
  }
