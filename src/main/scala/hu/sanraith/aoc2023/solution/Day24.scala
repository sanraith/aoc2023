package hu.sanraith.aoc2023.solution

import z3.scala._
import scala.util.Using
import scala.util.Using.Releasable

/** Solution for https://adventofcode.com/2023/day/24 */
class Day24 extends Solution:

  var part1XYRange = 200000000000000L to 400000000000000L

  override val title: String = "Never Tell Me The Odds"

  override def part1(ctx: Context): Int =
    val hailstones = parseInput(ctx.input)
    val tRange = part1XYRange
    hailstones.combinations(2).count { case Seq(sa, sb) =>
      intersection(sa, sb) match
        case (None, _) => false
        case (Some((x, y)), _) =>
          tRange.start <= x && x <= tRange.end && tRange.start <= y && y <= tRange.end
    }

  override def part2(ctx: Context): Long =
    val hailstones = parseInput(ctx.input)
    z3FindTrajectory(hailstones.take(3)) match
      case Some(Seq(rx, ry, rz, _, _, _)) => rx + ry + rz
      case _                              => throw new Exception("Z3 - Unsatisfiable constraints")

  def z3FindTrajectory(stones: Seq[Hailstone]) =
    val result = Using(new Z3Context()): ctx =>
      val solver = ctx.mkSolver()
      val Seq(rx, ry, rz, vrx, vry, vrz) = Seq("rx", "ry", "rz", "vrx", "vry", "vrz")
        .map(ctx.mkIntConst(_))
      val outVariables = OutVariables(rx, ry, rz, vrx, vry, vrz)
      stones.foreach(hs => z3AddHailstoneEquations(hs, outVariables, ctx, solver))

      if (solver.check() == Some(true))
        val model = solver.getModel()
        Some:
          Seq(rx, ry, rz, vrx, vry, vrz)
            .map(model.eval(_, true).get)
            .map(ctx.getNumeralReal(_).numerator.longValue)
      else None
    result.toOption.flatten

  def z3AddHailstoneEquations(
      h1: Hailstone,
      outVariables: OutVariables,
      ctx: Z3Context,
      solver: Z3Solver
  ) =
    val t1 = ctx.mkIntConst(s"t${h1.id}")
    solver.assertCnstr(ctx.mkGE(t1, ctx.mkInt(0, ctx.mkIntSort())))

    val x1 = ctx.mkNumeral(h1.pos.x.toString, ctx.mkIntSort())
    val vx1 = ctx.mkNumeral(h1.vel.x.toString, ctx.mkIntSort())
    val fx1 = ctx.mkAdd(x1, ctx.mkMul(t1, vx1))

    val y1 = ctx.mkNumeral(h1.pos.y.toString, ctx.mkIntSort())
    val vy1 = ctx.mkNumeral(h1.vel.y.toString, ctx.mkIntSort())
    val fy1 = ctx.mkAdd(y1, ctx.mkMul(t1, vy1))

    val z1 = ctx.mkNumeral(h1.pos.z.toString, ctx.mkIntSort())
    val vz1 = ctx.mkNumeral(h1.vel.z.toString, ctx.mkIntSort())
    val fz1 = ctx.mkAdd(z1, ctx.mkMul(t1, vz1))

    val OutVariables(rx, ry, rz, vrx, vry, vrz) = outVariables
    solver.assertCnstr(ctx.mkEq(ctx.mkAdd(rx, ctx.mkMul(t1, vrx)), fx1))
    solver.assertCnstr(ctx.mkEq(ctx.mkAdd(ry, ctx.mkMul(t1, vry)), fy1))
    solver.assertCnstr(ctx.mkEq(ctx.mkAdd(rz, ctx.mkMul(t1, vrz)), fz1))

  def intersection(sa: Hailstone, sb: Hailstone): (Option[(Double, Double)], Boolean) =
    val Point(x1, y1, _) = sa.pos
    val Point(x2, y2, _) = sa.pos + sa.vel
    val Point(x3, y3, _) = sb.pos
    val Point(x4, y4, _) = sb.pos + sb.vel
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
    val d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    val t0 = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
    val u0 = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)
    if (d == 0) // parallel
      if (t0 == 0 && u0 == 0) ??? // overlapping
      else (None, true) // not overlapping
    else if (
      (0 <= t0 && 0 <= d || 0 <= -t0 && 0 <= -d) && (0 <= u0 && 0 <= d || 0 <= -u0 && 0 <= -d)
    )
      val t = t0 / d.toDouble
      val x = x1 + t * (x2 - x1)
      val y = y1 + t * (y2 - y1)
      (Some((x, y)), false)
    else (None, false)

  def parseInput(input: String) =
    val numberRegex = """-?\d+""".r
    input.linesIterator.zipWithIndex
      .map: (line, i) =>
        val Seq(px, py, pz, vx, vy, vz) = numberRegex.findAllIn(line).map(_.toLong).toList
        Hailstone(i, Point(px, py, pz), Point(vx, vy, vz))
      .toSeq

  case class OutVariables(rx: Z3AST, ry: Z3AST, rz: Z3AST, vrx: Z3AST, vry: Z3AST, vrz: Z3AST)
  case class Hailstone(id: Int, pos: Point, vel: Point)
  case class Point(x: Long, y: Long, z: Long):
    def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
    def -(other: Point): Point = Point(x - other.x, y - other.y, z - other.z)
    def *(factor: Long): Point = Point(x * factor, y * factor, z * factor)
    def /(factor: Long): Point = Point(x / factor, y / factor, z / factor)

  given Releasable[Z3Context] with {
    def release(resource: Z3Context): Unit = resource.delete()
  }
