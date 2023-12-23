package hu.sanraith.aoc2023.solution

import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/23 */
class Day23 extends Solution:
  val enableDebugPrint = false

  override val title: String = "A Long Walk"

  // This uses the fact that there are no loops in the input
  override def part1(ctx: Context): Int =
    val (start, end, layout) = parseLayout(ctx.input)
    val queue = mut.Queue((start, South, 0))
    val visited = mut.Map.empty[Point, Int]
    while (queue.length > 0) do
      val (pos, dir, dist) = queue.dequeue()
      visited.get(pos) match
        case Some(prevDist) if prevDist >= dist => ()
        case _ =>
          visited(pos) = dist
          val nextDirs = layout.path(pos) match
            case '>' => Seq(East)
            case 'v' => Seq(South)
            case '<' => Seq(West)
            case '^' => Seq(North)
            case '.' => Directions
            case _   => throw new Exception("logic error")
          nextDirs
            .map(d => (pos + d, d))
            .filter((p, d) => layout.path.contains(p) && d * -1 != dir)
            .foreach((p, d) => queue.enqueue((p, d, dist + 1)))
    visited(end)

  override def part2(ctx: Context): Int =
    val (startPoint, endPoint, layout) = parseLayout(ctx.input)
    val nodes = parseNodes(startPoint, endPoint, layout)
    val nodeCount = nodes.size.toDouble
    val start = nodes(startPoint)
    val end = nodes(endPoint)

    var longest = 0
    val queue = mut.Queue((start, Set.empty[Node], 0))
    while (queue.length > 0) do
      val (node, path, dist) = queue.dequeue()
      ctx.progress(path.size / nodeCount)
      if (node == end) //
        longest = math.max(longest, dist)
      else
        val pathNext = path + node
        node.edges
          .filter((d, n) => !pathNext.contains(n))
          .foreach((d, n) => queue.enqueue((n, pathNext, dist + d)))
    longest

  def parseNodes(start: Point, end: Point, layout: Layout) =
    val queue = mut.Queue((start, 0, South, None: Option[Node]))
    val nodes = mut.Map.empty[Point, Node]
    while (queue.length > 0) do
      val (pos, dist, dir, fromNode) = queue.dequeue()
      val nextPair = if (Directions.map(pos + _).filter(layout.path.contains(_)).size != 2)
        val newNode = !nodes.contains(pos)
        val node = nodes.getOrElseUpdate(pos, Node(pos))
        fromNode.foreach: fromNode =>
          fromNode.edges.addOne(dist, node)
          node.edges.addOne(dist, fromNode)
        if (newNode) Some(1, Some(node)) else None
      else Some(dist + 1, fromNode)

      nextPair.foreach: (nextDist, nextFrom) =>
        Directions
          .map(d => (pos + d, d))
          .filter: (p, d) =>
            layout.path.contains(p) && d * -1 != dir && fromNode.forall(f => f.pos != p)
          .foreach((p, d) => queue.enqueue((p, nextDist, d, nextFrom)))
    end while

    if (enableDebugPrint)
      debugPrint(layout, nodes.keySet.toSet)
      nodes.values.toSeq
        .sortBy(n => (n.pos.y, n.pos.x))
        .foreach: n =>
          println(s"${n.pos} -> ${n.edges.map((d, p) => s"${p.pos}:$d").mkString(", ")}")

    nodes.toMap

  def parseLayout(input: String) =
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    val path = input.linesIterator.zipWithIndex
      .flatMap: (l, y) =>
        l.zipWithIndex
          .map((c, x) => Point(x, y) -> c)
          .filter((_, c) => c != '#')
          .map((p, c) => p -> c)
      .toMap
    val sortedPath = path.keySet.toSeq.sortBy(p => (p.y, p.x))
    val start = sortedPath.head
    val end = sortedPath.last
    (start, end, Layout(path, width, height))

  def debugPrint(layout: Layout, path: Set[Point]): Unit =
    for (y <- 0 until layout.height)
      val line = (0 until layout.width).map: x =>
        val p = Point(x, y)
        if (path.contains(p)) 'O' else layout.path.get(p).getOrElse('#')
      println(line.mkString)
    println("")

  case class Layout(path: Map[Point, Char], width: Int, height: Int)

  case class Node(pos: Point):
    val edges = mut.Map.empty[Int, Node]

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
  val Directions = Seq(East, South, West, North)
