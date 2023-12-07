package hu.sanraith.aoc2023.solution

trait Context:
  val input: String
  def progress(value: Double): Unit = {}

type NumberOrString = Int | Long | String

abstract class Solution:
  val title: String
  def part1(ctx: Context): NumberOrString
  def part2(ctx: Context): NumberOrString
  var println: Any => Unit = msg => Predef.println(msg)

sealed class SolutionInfo(
    val classDef: Class[?],
    val day: Int
):
  def createInstance(): Solution = classDef.newInstance().asInstanceOf[Solution]
  override def toString(): String = s"Day #$day"

val SolutionMap =
  val dayRegex = """([1-9]\d*)$""".r
  SolutionClasses
    .map(c => (c, dayRegex.findFirstIn(c.getName()).map(_.toInt)))
    .collect { case (c, Some(day)) => (day, SolutionInfo(c, day)) }
    .toMap
