package hu.sanraith.aoc2023.solution

import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/21 */
class Day21 extends Solution:
  val enableDebugPrint = true
  var part1MaxSteps = 64
  var part2MaxSteps = 26501365

  override val title: String = "Step Counter"

  override def part1(ctx: Context): Int =
    val (start, layout) = parseLayout(ctx.input)
    countReachable(Seq((start, 0)), part1MaxSteps, layout)

  override def part2(ctx: Context): Long =
    val (start, layout) = parseLayout(ctx.input)
    val maxSteps = part2MaxSteps
    // if (maxSteps < 26501365)
    //   return countReachableRepeating(Seq((start, 0)), maxSteps, layout)._1

    if (enableDebugPrint)
      debugPrint(
        layout,
        countReachableRepeating(Seq((start, 0)), maxSteps, layout)._2.toVector,
        maxSteps
      )

    val horizontalRocks =
      (0 to layout.width).filter(x => layout.rocks.contains(Point(x, start.y))).size
    val verticalRocks =
      (0 to layout.height).filter(y => layout.rocks.contains(Point(start.x, y))).size

    println(s"width: ${layout.width}, height: ${layout.height}")
    println(s"start: $start")
    println(s"rocks in start row/cloumn: $horizontalRocks, $verticalRocks")
    // In the real input there are no rocks in the row and column of the start position.
    // Because of this, each quadrant will be filled from the exact middle of (top, bottom, left, right),
    // the diagonals from 2 sides at once. As they fill the exact same way, to look at the 4 different kinds
    //   |
    //  +++
    // -+++-
    //  +++
    //   |
    // There is also the property that the reachable points depend on the parity of the index do to being able to
    // step forward and back. I do not rely on this completely since there can be unreachable walled-up places.
    //
    // So we need to count the fully filled blocks, and the blocks on the edge.
    //    T
    //   4X1
    //  4XXX1
    // LXXMXXR
    //  3XXX2
    //   3X2
    //    B
    // <-----> width = 7
    // val sidesCount = width / 2 - 1 // count of each side on the *border*, e.g. just the number of 2s on the *border*

    // the grid is also 131 wide, so the pattern is different in each row/col...
    //    1
    //   101
    //  10101
    // 1010101
    //  10101
    //   101
    //    1
    // This means there is a big difference between the count of each...
    // ALSO, there are smidges in between, so the diagonal line is not simple. see the dots:
    //   .1.
    //  .101.
    // .10101.
    // 1010101
    // .10101.
    //  .101.
    //   .1.

    val distanceTo1stQuadrant = layout.width - start.x
    println(s"distance to 1st quadrant: $distanceTo1stQuadrant")

    val completedQuadrants = (maxSteps - distanceTo1stQuadrant) / layout.width + 1
    val remainder = (maxSteps - distanceTo1stQuadrant) % layout.width
    println(s"completed quadrants to the right: $completedQuadrants, remainder: $remainder")

    // ceil to count the incomplete quadrants too
    val quadrantWidth =
      Math.ceil((maxSteps - distanceTo1stQuadrant) / layout.width.toDouble).toInt * 2 + 1
    val innerQuadrantWidth = quadrantWidth - 2
    val totalQuadrantArea = diamondArea(quadrantWidth)
    val innerQuadrantArea = diamondArea(innerQuadrantWidth)
    println(
      s"width: $quadrantWidth, innerWidth: $innerQuadrantWidth"
    )
    println(s"area: $totalQuadrantArea, innerArea: $innerQuadrantArea")

    val debugFills = mut.Map.empty[Point, Int]
    val (startQuadrantSteps, startFill) = countReachableBounded(Seq((start, 0)), maxSteps, layout)
    // debugFills ++= startFill
    println(s"Steps in start quadrant: $startQuadrantSteps")

    val oddBorderCount = Math.ceil((innerQuadrantWidth / 2 + 1) / 2.0).toInt
    // Math.ceil(innerQuadrantWidth / 2.0).toInt / 2
    // Math.ceil((innerQuadrantWidth + 1) / 2.0).toInt
    val evenBorderCount = Math.floor((innerQuadrantWidth / 2 + 1) / 2.0).toInt
    // Math.floor(innerQuadrantWidth / 2.0).toInt / 2
    println(s"oddBorderCount: $oddBorderCount, evenBorderCount: $evenBorderCount")

    val (evenInnerBorderSteps, evenFill) =
      countReachableBounded(Seq((Point(0, start.y), layout.width)), maxSteps, layout)
    val (oddInnerBorderSteps, oddFill) =
      countReachableBounded(Seq((Point(0, start.y), 0)), maxSteps, layout)

    println("Odd:")
    debugPrint(layout, oddFill.toVector, maxSteps)
    println("Even:")
    debugPrint(layout, evenFill.toVector, maxSteps)

    println(s"oddInnerBorderSteps: $oddInnerBorderSteps")
    println(s"evenInnerBorderSteps: $evenInnerBorderSteps")
    val oddQuadrantCount = sumOfSequence(oddBorderCount, oddSequence)
    val evenQuadrantCount = sumOfSequence(evenBorderCount, evenSequence)
    println(s"oddQuadrantCount: $oddQuadrantCount, evenQuadrantCount: $evenQuadrantCount")
    val innerArea =
      oddQuadrantCount * oddInnerBorderSteps + evenQuadrantCount * evenInnerBorderSteps
    println(s"innerArea: $innerArea")

    val sideFactor = quadrantWidth.toLong / 2 - 1
    println(s"sideFactor: $sideFactor")
    val startPositions = Seq(
      (Point(0, start.y), East),
      (Point(start.x, 0), South),
      (Point(layout.width - 1, start.y), West),
      (Point(start.x, layout.height - 1), North)
    )

    val diagonalFills = (startPositions :+ startPositions.head)
      .sliding(2)
      .map: pairss =>
        val pairs = pairss.map(_._1)
        val dir = pairss(0)._2
        val currentSteps = maxSteps - remainder

        // val (_, innerFill) =
        //   countReachableBounded(Seq((diagonalMid, diagonalMid.x - start.x)), maxSteps, layout)
        val innerFill =
          oddFill.map((p, v) =>
            p -> (v + (quadrantWidth / 2 - 1) * layout.width /* - layout.width / 2*/ )
          )

        val side1 = getSidePlus1(dir, innerFill, maxSteps, layout)
        val side2 = getSidePlus1(cw(dir), innerFill, maxSteps, layout)
        val (area, sideFill) = countReachableBounded(side2 ++ side2, maxSteps, layout)
        // val (area, sideFill) = countReachableBounded(pairs.map((_, currentSteps)), maxSteps, layout)

        if (dir == East)

          println(s"INNERFILL ---")
          println(s"max x: ${innerFill.map(_._2).max}")
          debugPrint(layout, innerFill.toVector, maxSteps)

          // println(s"SIDES ---")
          // debugPrint(layout, (side1 ++ side2).toVector, maxSteps)

          println(s"SIDEFILL ---")
          debugPrint(layout, sideFill.toVector, maxSteps)

        println(s"Steps from $pairs: ${area}, startingSteps: ${currentSteps}")
        (dir, sideFill, area * sideFactor)
      .toSeq
    val diagonalBigArea = diagonalFills.map(_._3).sum
    debugFills ++= diagonalFills.flatMap(_._2)
    println(s"diagonalArea: ${diagonalBigArea}")

    val pointyFills = startPositions
      .map: (start, dir) =>
        val currentSteps = maxSteps - remainder
        val (area, sideFill) = countReachableBounded(Seq((start, currentSteps)), maxSteps, layout)
        println(s"Steps from $start: $area, startingSteps: ${currentSteps}")

        // debugPrint(layout, sideFill.toVector, maxSteps)

        (dir, sideFill, area)
    val pointyArea = pointyFills.map(_._3).sum
    debugFills ++= pointyFills.flatMap(_._2)
    println(s"pointyArea: $pointyArea")

    val diagonalSmallAreas = diagonalFills.zipWithIndex
      .map: (value, i) =>
        val (dir, sideFill, _) = value
        val side1 = getSidePlus1(dir, sideFill, maxSteps, layout)
        val side2 = getSidePlus1(cw(dir), sideFill, maxSteps, layout)
        val (midArea, midFill) = countReachableBounded(side2 ++ side2, maxSteps, layout)
        val midAreas = midArea * (sideFactor - 1)
        debugFills ++= midFill
        // println(s"diagonal dir $dir")
        // println(s"side1: ${side1.size}, side2: ${side2.size}, midAreas: ${midAreas}")

        val (_, ppFills, _) = pointyFills(i)
        val pSide = getSidePlus1(cw(dir), ppFills, maxSteps, layout)
        val (ppArea, ppFill) = countReachableBounded(side1 ++ pSide, maxSteps, layout)
        debugFills ++= ppFills
        // println(s"ppSide: ${pSide.size}, ppArea: ${ppArea}")

        val (_, npFills, _) = pointyFills((i + 1) % 4)
        val nSide = getSidePlus1(dir, npFills, maxSteps, layout)
        val (npArea, npFill) = countReachableBounded(side2 ++ nSide, maxSteps, layout)
        debugFills ++= npFills
        println(s"ppSide: ${nSide.size}, ppArea: ${npArea}")

        val sum = midAreas + ppArea + npArea
        // println(s"sum: $sum")

        // println("--------------")
        // debugPrint(layout, midFill.toVector, maxSteps)
        // debugPrint(layout, ppFill.toVector, maxSteps)
        // debugPrint(layout, npFill.toVector, maxSteps)

        sum
      .sum

    // debugPrint(layout, debugFills.toVector)

    val sum = /*startQuadrantSteps +*/ innerArea + diagonalBigArea + diagonalSmallAreas + pointyArea
    if (
      sum <= 608193750177626L ||
      sum >= 608194125790724L
      // sum <= 631356785795824L ||
      // sum <= 631356785803554L ||
      // sum <= 631356831523448L ||
      // sum == 631357583266212L ||
      // sum == 631357550902312L ||
      // sum == 631357569311516L
    )
      // 631356785795824 too low
      println("BAD RESULT")

    sum

  def cw(dir: Direction) = Directions((Directions.indexOf(dir) + 1) % 4)
  def ccw(dir: Direction) = Directions((Directions.indexOf(dir) + 3) % 4)

  def getSidePlus1(dir: Direction, points: mut.Map[Point, Int], maxSteps: Int, layout: Layout) =
    val (xRange, yRange) = dir match
      case North => ((0 until layout.width), (0 to 0))
      case East  => ((layout.width - 1 to layout.width - 1), (0 until layout.height))
      case South => ((0 until layout.width), (layout.height - 1 to layout.height - 1))
      case West  => ((0 to 0), (0 to layout.height))
    (for
      y <- yRange
      x <- xRange
    yield (Point(x, y)))
      .flatMap(p => if (points.contains(p)) Some(p -> points(p)) else None)
      .map((p, v) => (p + dir) -> (v + 1))
      .flatMap: (p, v) =>
        if (v <= maxSteps && !layout.rocks.contains(normalizePoint(p, layout)))
          Some(normalizePoint(p, layout) -> v)
        else None
      .toSeq

  def evenSequence(n: Int): Long = 8 * n - 4
  def oddSequence(n: Int): Long = if (n == 1) 1 else 8 * (n - 1)
  def sumOfSequence(n: Int, itemAt: Int => Long) = n * (itemAt(1) + itemAt(n)) / 2

  def diamondArea(width: Int) =
    val height = (width / 2) + 1
    (width + 1) * height - width

  def countReachableBounded(starts: Seq[(Point, Int)], maxSteps: Int, layout: Layout) =
    val plots = (for
      x <- (0 until layout.width)
      y <- (0 until layout.height)
    yield (Point(x, y))).filter(!layout.rocks.contains(_)).toSet

    val queue = mut.Queue(starts: _*)
    val visited = mut.Map.empty[Point, Int]
    while (queue.length > 0) do
      val (pos, stepCount) = queue.dequeue()
      if (!visited.contains(pos))
        visited.addOne((pos, stepCount))
        if (stepCount < maxSteps)
          Directions
            .map(pos + _)
            .filter(plots.contains(_))
            .foreach(nextPos => queue.enqueue((nextPos, stepCount + 1)))

    val visitedByExactSteps = visited.filter((k, v) => v % 2 == maxSteps % 2) // TODO check
    // debugPrint(layout, visitedByExactSteps.toVector)
    (visitedByExactSteps.size, visited) // .size

  def countReachableRepeating(starts: Seq[(Point, Int)], maxSteps: Int, layout: Layout) =
    val queue = mut.Queue(starts: _*)
    val visited = mut.Map.empty[Point, Int]
    while (queue.length > 0) do
      val (pos, stepCount) = queue.dequeue()
      if (!visited.contains(pos))
        visited.addOne((pos, stepCount))
        if (stepCount < maxSteps)
          Directions
            .map(pos + _)
            .filter(p => !layout.rocks.contains(normalizePoint(p, layout)))
            .foreach(nextPos => queue.enqueue((nextPos, stepCount + 1)))
    val visitedByExactSteps = visited.filter((k, v) => v % 2 == maxSteps % 2)
    // debugPrint(layout, visitedByExactSteps.toVector)
    (visitedByExactSteps.size, visited)

  def countReachable(starts: Seq[(Point, Int)], maxSteps: Int, layout: Layout) =
    val queue = mut.Queue(starts: _*)
    val visited = mut.Map.empty[Point, Int]
    while (queue.length > 0) do
      val (pos, stepCount) = queue.dequeue()
      if (!visited.contains(pos))
        visited.addOne((pos, stepCount))
        if (stepCount < maxSteps)
          Directions
            .map(pos + _)
            .filter(!layout.rocks.contains(_))
            .foreach(nextPos => queue.enqueue((nextPos, stepCount + 1)))

    val visitedByExactSteps = visited.filter((k, v) => v % 2 == maxSteps % 2)
    // debugPrint(layout, visitedByExactSteps.toVector)
    visitedByExactSteps.size

  def getQuadrant(p: Point, layout: Layout) =
    Point(
      (if (p.x < 0) p.x - layout.width + 1 else p.x) / layout.width,
      (if (p.y < 0) p.y - layout.height + 1 else p.y) / layout.height
    )

  def normalizePoint(p: Point, layout: Layout) =
    val Layout(_, width, height) = layout
    var Point(x, y) = p
    x = (x + (math.abs(x) / width + 1) * width) % width
    y = (y + (math.abs(y) / height + 1) * height) % height
    Point(x, y)

  def debugPrint(
      layout: Layout,
      path: Vector[(Point, Int)],
      maxSteps: Int,
      force: Boolean = false
  ): Unit =
    if (path.isEmpty)
      println("empty area")
      return
    else if (!force && !enableDebugPrint) return

    val minX = path.map((p, _) => p.x).min
    val minY = path.map((p, _) => p.y).min
    val maxX = path.map((p, _) => p.x).max
    val maxY = path.map((p, _) => p.y).max

    val pathMap = path.toMap
    for (y <- minY to maxY)
      val line = (minX to maxX).map: x =>
        val p = Point(x, y)
        val normP = normalizePoint(p, layout)
        pathMap.get(p) match
          case _
              if (normP.x == 0 || normP.x == layout.width - 1) && (normP.y != 0 && normP.y != layout.height - 1) =>
            '│'
          case _
              if (normP.y == 0 || normP.y == layout.height - 1) && (normP.x != 0 && normP.x != layout.width - 1) =>
            '─'
          case _
              if normP.x == 0 || normP.y == 0 || normP.x == layout.width - 1 || normP.y == layout.height - 1 =>
            '┼'
          case Some(v) => if (v % 2 == maxSteps % 2) 'O' else '░' // █▓▒░
          case _       => if (layout.rocks.contains(normP)) '.' /*'#'*/ else ' ' // '.'
      println(line.mkString)
    println("")
  end debugPrint

  def parseLayout(input: String) =
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    var start = Point(0, 0)
    val rocks = input.linesIterator.zipWithIndex
      .flatMap: (l, y) =>
        l.zipWithIndex
          .map((c, x) => Point(x, y) -> c)
          .tapEach((p, c) => if (c == 'S') start = p)
          .filter((_, c) => c == '#')
          .map((p, _) => p)
      .toSet
    (start, Layout(rocks, width, height))

  case class Layout(rocks: Set[Point], width: Int, height: Int)

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)
    def *(other: Point): Point = Point(x * other.x, y * other.y)
    def *(scalar: Int): Point = Point(x * scalar, y * scalar)
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs

  type Direction = Point
  val East: Direction = Point(1, 0)
  val South: Direction = Point(0, 1)
  val West: Direction = Point(-1, 0)
  val North: Direction = Point(0, -1)
  val Center: Direction = Point(0, 0)
  val Directions = Seq(East, South, West, North)
