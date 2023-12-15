package hu.sanraith.aoc2023.solution

import scala.collection.mutable

/** Solution for https://adventofcode.com/2023/day/15 */
class Day15 extends Solution:
  override val title: String = "Lens Library"

  override def part1(ctx: Context): Int =
    val steps = ctx.input.linesIterator.flatMap(_.split(","))
    steps.map(hash).sum

  override def part2(ctx: Context): Int =
    val stepRegex = """([a-z]+)([-=])(\d*)""".r
    val steps = ctx.input.linesIterator
      .flatMap(_.split(","))
      .map { case stepRegex(label, op, focalLength) =>
        Step(label, op(0), focalLength.toIntOption.getOrElse(0))
      }

    val boxes = (0 until 256).map(_ => mutable.SeqMap[String, Step]())
    for (step <- steps)
      val box = boxes(hash(step.label))
      step.operation match
        case '-' => box.remove(step.label)
        case '=' => box.addOne((step.label, step))

    boxes.zipWithIndex
      .flatMap: (box, boxNumber) =>
        box.values.zipWithIndex.map: (lens, slotNumber) =>
          (boxNumber + 1) * (slotNumber + 1) * lens.focalLength
      .sum

  def hash(step: String): Int =
    step.foldLeft(0)((acc, c) => ((acc + c.toByte) * 17) % 256)

  case class Step(label: String, operation: Char, focalLength: Int)
