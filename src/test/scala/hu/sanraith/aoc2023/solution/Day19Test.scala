package hu.sanraith.aoc2023.solution
import hu.sanraith.aoc2023.SolutionTestSpec

/** Tests for https://adventofcode.com/2023/day/19 */
class Day19Test extends SolutionTestSpec:

  describe("Day19 for example input") {
    given day: Solution = Day19()

    it("solves part 1"):
      assertPart(day.part1, expected = 19114, input = example)

    it("solves part 2"):
      assertPart(day.part2, expected = 167409079868000L, input = example)
  }

  describe("Day19 for puzzle input") {
    given day: Solution = Day19()

    it("solves part 1")(_assertPart(day.part1, 350678))
    it("solves part 2")(_assertPart(day.part2, 124831893423809L))
  }

  val example = """
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"""
