package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/17 */
class Day17Test extends SolutionTestSpec:

  describe("Day17 for example input") {
    given day: Solution = Day17()

    it("solves part 1"):
      assertPart(day.part1, expected = 102, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 94, input = example)
      assertPart(
        day.part2,
        expected = 71,
        input = """
111111111111
999999999991
999999999991
999999999991
999999999991"""
      )
  }

  describe("Day17 for puzzle input") {
    given day: Solution = Day17()

    it("solves part 1")(assertPart(day.part1, 785))
    it("solves part 2")(assertPart(day.part2, 922))
  }

  val example = """
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"""
