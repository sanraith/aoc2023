package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/2 */
class Day02Test extends SolutionTestSpec:

  describe("Day02 for example input") {
    given day: Solution = Day02()

    it("solves part 1"):
      assertPart(day.part1, expected = 8, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 2286, input = example)
  }

  describe("Day02 for puzzle input") {
    given day: Solution = Day02()

    it("solves part 1")(assertPart(day.part1, 2256))
    it("solves part 2")(assertPart(day.part2, 74229))
  }

  val example = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
