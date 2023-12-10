package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/10 */
class Day10Test extends SolutionTestSpec:

  describe("Day10 for example input") {
    given day: Solution = Day10()

    it("solves part 1"):
      assertPart(
        day.part1,
        expected = 8,
        input = """
..F7.
.FJ|.
SJ.L7
|F--J
LJ..."""
      )

    it("solves part 2 example 1"):
      assertPart(
        day.part2,
        expected = 4,
        input = """
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."""
      )

    it("solves part 2 example 2"):
      assertPart(
        day.part2,
        expected = 10,
        input = """
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"""
      )
  }

  describe("Day10 for puzzle input") {
    given day: Solution = Day10()

    it("solves part 1")(assertPart(day.part1, 6909))
    it("solves part 2")(assertPart(day.part2, 461))
  }
