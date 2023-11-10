package hu.sanraith.aoc2023.solution

trait Context:
  val input: String
  def progress(value: Double): Unit = {}

abstract class Solution:
  val title: String
  def part1(ctx: Context): String
  def part2(ctx: Context): String

sealed class SolutionInfo(
    val classDef: Class[?],
    val day: Int
):
  def createInstance(): Solution = classDef.newInstance().asInstanceOf[Solution]
  override def toString(): String = s"Day #$day"

val solutionMap =
  val dayRegex = """([1-9]\d*)$""".r
  solutionClasses
    .map(c => (c, dayRegex.findFirstIn(c.getName()).map(_.toInt)))
    .collect { case (c, Some(day)) => (day, SolutionInfo(c, day)) }
    .toMap
