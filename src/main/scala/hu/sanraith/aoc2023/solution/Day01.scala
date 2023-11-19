package hu.sanraith.aoc2023.solution

class Day01 extends Solution:
  override val title: String = "Unknown Title"

  override def part1(ctx: Context): String =
    for (x <- Range(0, 10))
      Thread.sleep(100)
      println(s"debug message $x")
    "abcde"

  override def part2(ctx: Context): String =
    "1234"
