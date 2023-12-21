package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/21 */
class Day21Test extends SolutionTestSpec:

  describe("Day21 for example input") {
    given day: Solution = Day21()

    // it("solves part 1 for 6 steps"):
    //   day.asInstanceOf[Day21].part1MaxSteps = 6
    //   assertPart(day.part1, expected = 16, input = example)

    // it("solves part 2 for 6 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 6
    //   assertPart(day.part2, expected = 16, input = example)
    // it("solves part 2 for 10 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 10
    //   assertPart(day.part2, expected = 50, input = example)
    // it("solves part 2 for 50 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 50
    //   assertPart(day.part2, expected = 1594, input = example)
    // it("solves part 2 for 100 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 100
    //   assertPart(day.part2, expected = 6536, input = example)
    // it("solves part 2 for 500 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 500
    //   assertPart(day.part2, expected = 167004, input = example)
    // it("solves part 2 for 1000 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 1000
    //   assertPart(day.part2, expected = 668697, input = example)
    // // it("solves part 2 for 5000 steps"):
    // //   day.asInstanceOf[Day21].part2MaxSteps = 5000
    // //   assertPart(day.part2, expected = 16733044, input = example)
  }

  describe("Day21 for custom input") {
    given day: Solution = Day21()

    // it("solves part 2 for 27 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 27
    //   assertPart(day.part2, expected = 588, input = part2CustomExample)
    it("solves part 2 for 50 steps"):
      day.asInstanceOf[Day21].part2MaxSteps = 4 * 11 + 5
      assertPart(day.part2, expected = 1878, input = part2CustomExample)
    // it("solves part 2 for 100 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 100
    //   assertPart(day.part2, expected = 7645, input = part2CustomExample)
    // it("solves part 2 for 500 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 500
    //   assertPart(day.part2, expected = 188756, input = part2CustomExample)
    // it("solves part 2 for 1000 steps"):
    //   day.asInstanceOf[Day21].part2MaxSteps = 1000
    //   assertPart(day.part2, expected = 753480, input = part2CustomExample)
  }

  describe("Day21 for puzzle input") {
    given day: Solution = Day21()

    // it("solves part 1")(assertPart(day.part1, 3788))
    it("solves part 2")(_assertPart(day.part2, "__PART_2_EXPECTED__"))
  }

  val example = """
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."""

  val part2CustomExample = """
...........
......##.#.
.###..#..#.
..#.#...#..
....#.#....
.....S.....
.##......#.
.......##..
.##.#.####.
.##...#.##.
..........."""
