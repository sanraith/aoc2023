package hu.sanraith.aoc2023.solution

import scala.collection.{mutable => mut}

/** Solution for https://adventofcode.com/2023/day/25 */
class Day25 extends Solution:
  override val title: String = "Snowverload"

  override def part1(ctx: Context): Int =
    val connections = parseConnections(ctx.input)
    val nodes = flood(connections, ctx)
    val (farthestA, farthestB, d) = nodes.iterator
      .flatMap((from, v) => v.distanceTo.map((to, d) => (from, to, d)))
      .max(Ordering.by(_._3))

    var groupPairs = Vector(
      (Set(farthestA), Set(farthestA)),
      (Set(farthestB), Set(farthestB))
    )
    var intersection = Set.empty[String]
    while (intersection.size == 0) do
      val nexts = groupPairs.map: (group, checks) =>
        checks.flatMap(nodes(_).neighbours.filter(!group.contains(_))).toSet
      intersection = nexts(0).intersect(nexts(1))
      groupPairs = groupPairs.map(_._1).zip(nexts).map((g, n) => (g ++ n.diff(intersection), n))
    var groups = groupPairs.map(_._1)

    assert(intersection.size == 3)
    val cutOptions = intersection.toSeq.map(a => nodes(a).neighbours.toSeq.map(b => (a, b)))
    val cuts = (for
      a <- cutOptions(0)
      b <- cutOptions(1)
      c <- cutOptions(2)
    yield {
      val forbidden = Seq(a, b, c).iterator.flatMap((from, to) => Seq((from, to), (to, from))).toSet
      if (tryReach(farthestA, farthestB, nodes, forbidden)) None
      else Some(Seq(a, b, c))
    }).flatten.head

    cuts.foreach: (a, b) =>
      groups = groups.map: g =>
        if (!g.contains(a) && !g.contains(b)) g ++ Set(a, b).intersect(intersection) else g

    groups.map(_.size).product

  override def part2(ctx: Context): String = "*"

  def tryReach(
      start: String,
      end: String,
      nodes: Map[String, Node],
      forbidden: Set[(String, String)]
  ): Boolean =
    val visited = mut.Set.empty[String]
    val queue = mut.Queue(start)
    val startNode = nodes(start)
    var isFound = false
    while (!isFound && queue.length > 0) do
      val name = queue.dequeue()
      if (name == end) isFound = true
      else if (visited.add(name))
        nodes(name).neighbours
          .filter(to => !visited.contains(to) && !forbidden.contains((name, to)))
          .foreach(next => queue.addOne(next))
    isFound

  def flood(connections: Map[String, Set[String]], ctx: Context) =
    val nodes = connections.map((k, v) => k -> Node(k, v.toSet, mut.Map.empty))
    val progressMax = connections.size * 1.01
    for ((start, i) <- connections.keySet.toSeq.zipWithIndex)
      ctx.progress(i / progressMax)
      val visited = mut.Set.empty[String]
      val queue = mut.Queue((start, 0))
      val startNode = nodes(start)
      while (queue.length > 0) do
        val (name, dist) = queue.dequeue()
        if (visited.add(name))
          if (start != name) startNode.distanceTo(name) = dist
          nodes(name).neighbours
            .filter(!visited.contains(_))
            .foreach(next => queue.addOne((next, dist + 1)))
    nodes

  def parseConnections(input: String) =
    val nameRegex = """\w+""".r
    val map = mut.Map.empty[String, mut.Set[String]]
    input.linesIterator.foreach: line =>
      val Array(name, partStr) = line.split(":")
      val parts = nameRegex.findAllIn(partStr).toSeq
      map.getOrElseUpdate(name, mut.Set.empty).addAll(parts)
      parts.foreach(p => map.getOrElseUpdate(p, mut.Set.empty).add(name))
    map.map((k, v) => k -> v.toSet).toMap

  case class Edge(a: String, b: String)

  case class Node(name: String, neighbours: Set[String], distanceTo: mut.Map[String, Int])
