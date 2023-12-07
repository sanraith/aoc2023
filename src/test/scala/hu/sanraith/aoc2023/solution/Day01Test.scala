package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/1 */
class Day01Test extends SolutionTestSpec:

  describe("Day01 for example input") {
    given day: Solution = Day01()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = 142,
        input = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""
      )

    it("solves part 2"):
      assertPart(
        day.part2,
        expected = 281,
        input = """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""
      )
  }

  describe("Day01 for custom input") {
    given day: Solution = Day01()

    it("works with overlapping text at the end of the line for part 2"):
      assertPart(day.part2, expected = 12, input = "1eightwo")
  }

  describe("Day01 for puzzle input") {
    given day: Solution = Day01()

    it("solves part 1")(assertPart(day.part1, 54667))
    it("solves part 2")(assertPart(day.part2, 54203))
  }
