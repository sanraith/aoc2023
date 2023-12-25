package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/25 */
class Day25Test extends SolutionTestSpec:

  describe("Day25 for example input") {
    given day: Solution = Day25()

    it("solves part 1"):
      assertPart(day.part1, expected = 54, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = "*", input = example)
  }

  describe("Day25 for puzzle input") {
    given day: Solution = Day25()

    it("solves part 1")(assertPart(day.part1, 558376))
    it("solves part 2")(assertPart(day.part2, "*"))
  }

  val example = """
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr"""
