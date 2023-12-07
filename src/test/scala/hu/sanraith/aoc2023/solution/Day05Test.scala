package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/5 */
class Day05Test extends SolutionTestSpec:

  describe("Day05 for example input") {
    given day: Solution = Day05()

    it("solves part 1"):
      assertPart(day.part1, expected = 35, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 46, input = example)
  }

  describe("Day05 for puzzle input") {
    given day: Solution = Day05()

    it("solves part 1")(assertPart(day.part1, 993500720))
    it("solves part 2")(assertPart(day.part2, 4917124))
  }

  val example = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""
