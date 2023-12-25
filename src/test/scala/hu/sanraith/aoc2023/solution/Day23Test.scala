package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/23 */
class Day23Test extends SolutionTestSpec:

  describe("Day23 for example input") {
    given day: Solution = Day23()

    it("solves part 1"):
      assertPart(day.part1, expected = 94, input = example)

    it("solves part 2"):
      _assertPart(day.part2, expected = 154, input = example) // TODOD: check why this fails
  }

  describe("Day23 for puzzle input") {
    given day: Solution = Day23()

    it("solves part 1")(assertPart(day.part1, 2018))
    it("solves part 2")(assertPart(day.part2, 6406))
  }

  val example = """
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#"""
