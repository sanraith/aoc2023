package hu.sanraith.aoc2023.solution

import scala.collection.mutable

/** Solution for https://adventofcode.com/2023/day/8 */
class Day08 extends Solution:
  override val title: String = "Haunted Wasteland"

  override def part1(ctx: Context): Int =
    val (directions, map) = parseMap(ctx.input)
    val directionsSize = directions.size

    val end = "ZZZ"
    var current = map("AAA")
    var count = 0
    var dIdx = -1
    while current.name != end do
      count += 1
      dIdx = (dIdx + 1) % directionsSize
      current = directions(dIdx) match
        case 'L' => current.left
        case 'R' => current.right

    count

  override def part2(ctx: Context): Long =
    val (directions, map) = parseMap(ctx.input)
    val directionsSize = directions.size

    val junctions = mutable.Seq(map.values.filter(_.isP2Start).toSeq: _*)
    val cycles = mutable.Seq[Option[Long]](junctions.map(_ => None).toSeq: _*)
    var count = 0L
    var dIdx = -1
    while cycles.exists(_.isEmpty) do
      // I dont think this would work for any input, but for mine
      // the cycle size is just the position we encounter the first Z
      junctions.zipWithIndex
        .filter((c, i) => cycles(i).isEmpty && c.isP2End)
        .foreach((_, i) => cycles(i) = Some(count))

      count += 1
      dIdx = (dIdx + 1) % directionsSize
      for (i <- (0 until junctions.length))
        val current = junctions(i)
        junctions(i) = directions(dIdx) match
          case 'L' => current.left
          case 'R' => current.right

    cycles.flatten.foldLeft(1L)((acc, x) => leastCommonMultiple(acc, x))
  end part2

  def parseMap(input: String) =
    val directions = input.linesIterator.take(1).toSeq.head
    val junctionRegex = """(\w+) = \((\w+), (\w+)\)""".r
    val map = junctionRegex
      .findAllMatchIn(input)
      .map { case junctionRegex(name, left, right) => name -> Junction(name, left, right) }
      .toMap

    map.values.foreach: j =>
      j.left = map(j.leftName)
      j.right = map(j.rightName)

    (directions, map)

  def leastCommonMultiple(a: Long, b: Long): Long =
    (a * b).abs / greatestCommonDivisor(a, b)

  def greatestCommonDivisor(a: Long, b: Long): Long =
    if (b == 0) a else greatestCommonDivisor(b, a % b)

  case class Junction(
      val name: String,
      val leftName: String,
      val rightName: String
  ):
    val isP2Start = name.endsWith("A")
    val isP2End = name.endsWith("Z")
    var left: Junction = null
    var right: Junction = null

    override def toString: String = s"$name = ($leftName, $rightName)"
