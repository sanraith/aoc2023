package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/12 */
class Day12Test extends SolutionTestSpec:

  describe("Day12 for custom input") {
    given day: Solution = Day12()

    it("solves part 1 examples separately"):
      assertPart(day.part1, expected = 4, input = ".??..??..?##. 1,1,3")
      assertPart(day.part1, expected = 1, input = "???.### 1,1,3")
      assertPart(day.part1, expected = 4, input = ".??..??...?##. 1,1,3")
      assertPart(day.part1, expected = 1, input = "?#?#?#?#?#?#?#? 1,3,1,6")
      assertPart(day.part1, expected = 1, input = "????.#...#... 4,1,1")
      assertPart(day.part1, expected = 4, input = "????.######..#####. 1,6,5")
      assertPart(day.part1, expected = 10, input = "?###???????? 3,2,1")
  }

  describe("Day12 for example input") {
    given day: Solution = Day12()

    it("solves part 1"):
      assertPart(day.part1, expected = 21, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 525152, input = example)
  }

  describe("Day12 for puzzle input") {
    given day: Solution = Day12()

    it("solves part 1")(assertPart(day.part1, 7705))
    it("solves part 2")(assertPart(day.part2, 50338344809230L))
  }

  val example = """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""
