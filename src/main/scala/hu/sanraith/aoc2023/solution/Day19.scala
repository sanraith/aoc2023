package hu.sanraith.aoc2023.solution

import scala.collection.{mutable => mut}
import scala.collection.parallel.CollectionConverters._

/** Solution for https://adventofcode.com/2023/day/19 */
class Day19 extends Solution:
  override val title: String = "Aplenty"

  override def part1(ctx: Context): Int =
    val (workflows, parts) = parseInput(ctx.input)
    val pathRanges = getPathRanges(workflows)
    parts
      .filter: part =>
        val pr = pathRanges.find(rs => rs.forall((c, r) => r.contains(part(c))))
        pr match
          case Some(pr) =>
            // println(part)
            // println(rangesStr(pr))
            // println("")
            true
          case _ => false
      .map(p => p.values.sum)
      .sum

  override def part2(ctx: Context): Long =
    val (workflows, parts) = parseInput(ctx.input)
    val pathRanges = getPathRanges(workflows)
    val allRanges = {
      val rangeSets = "xmas".map(c => c -> mut.Set.empty[Int]).toMap
      pathRanges.foreach(cm => cm.foreach((c, r) => rangeSets(c).addAll(Seq(r.start, r.end))))
      rangeSets.map: (c, s) =>
        c -> s.toSeq.sorted.sliding(2).map { case Seq(a, b) => a until b }.toVector
    }
    // println(allRanges)
    // println("")

    // val foundCombinations = mut.Map.empty[Int, mut.ArrayBuffer[String]]

    // var sum = 0L
    // var i = 0
    // i += 1

    // val sum = (for {
    //   rx <- allRanges('x').par
    //   rm <- allRanges('m')
    //   ra <- allRanges('a')
    //   rs <- allRanges('s')
    // } yield {
    //   val yes = pathRanges.exists(pr =>
    //     pr('x').containsRange(rx) &&
    //       pr('m').containsRange(rm) &&
    //       pr('a').containsRange(ra) &&
    //       pr('s').containsRange(rs)
    //   )
    //   if (yes) 1L * rx.length * rm.length * ra.length * rs.length else 0L
    // }).sum

    var sum = 0L
    var i = 0
    for (rx <- allRanges('x'))
      ctx.progress(i.toDouble / allRanges('x').length)
      i += 1
      val rxRanges = pathRanges.filter(_('x').containsRange(rx))
      if (rxRanges.length > 0)
        for (rm <- allRanges('m'))
          val rmRanges = rxRanges.filter(_('m').containsRange(rm))
          if (rmRanges.length > 0)
            for (ra <- allRanges('a'))
              val raRanges = rmRanges.filter(_('a').containsRange(ra))
              if (raRanges.length > 0)
                for (rs <- allRanges('s'))
                  // val yes = pathRanges.exists: pr =>
                  //   pr('x').containsRange(rx) &&
                  //     pr('m').containsRange(rm) &&
                  //     pr('a').containsRange(ra) &&
                  //     pr('s').containsRange(rs)
                  if (raRanges.exists(_('s').containsRange(rs)))
                    sum += 1L * rx.length * rm.length * ra.length * rs.length
    sum

  // 206016000000000
  // 167409079868000
  // 3209582947500
  // 1110714862500
  // 9223372036854775807

  def getPathRanges(wfs: Map[String, Workflow]) =
    given workflows: Map[String, Workflow] = wfs
    val startRule = Rule(_ => true, "in", null, 0, 'x')
    val paths = findAcceptedPaths(mut.Stack(Seq(startRule)))
    paths.toSeq.zipWithIndex
      .map: (path, i) =>
        val ranges = mut.Map("xmas".map(c => c -> (1 until 4001)): _*)
        for (rules <- path)
          val appliedRules = rules.take(rules.length - 1).map(_.inverse) :+ rules.last
          for (rule <- appliedRules)
            lazy val r = ranges(rule.category)
            Option(rule.op) match
              case Some(">") => ranges.addOne((rule.category, rule.number + 1 until r.end))
              case Some("<") => ranges.addOne((rule.category, r.start until rule.number))
              case Some(_)   => throw new Exception("logic error")
              case None      => ()
        println(s"#$i ${path.map(_.last.target).mkString("->")}")
        println(rangesStr(ranges))
        println("")
        ranges.toMap
      .filter(ranges => ranges.values.forall(r => r.start <= r.end))
      .toVector

  def rangesStr(ranges: Iterable[(Char, Range)]) = ranges.toSeq
    .sortBy((c, _) => "xmas".indexOf(c))
    .map((c, r) => s"$c: ${r.start}..${r.end - 1}")
    .mkString(", ")

  def rangeStr(rx: Range): String = s"${rx.start}..${rx.end - 1}".padTo(11, ' ')

  def findAcceptedPaths(
      path: mut.Stack[Seq[Rule]],
      paths: mut.ArrayBuffer[Seq[Seq[Rule]]] = mut.ArrayBuffer.empty
  )(using workflows: Map[String, Workflow]): Iterable[Seq[Seq[Rule]]] =
    val wf = workflows(path.top.last.target)
    if (wf.isEnd)
      if (wf.isAccepted) {
        paths.addOne(path.toSeq.reverse)
      }
    else
      val currentRules = mut.ArrayBuffer.empty[Rule]
      for (next <- wf.rules)
        currentRules.addOne(next)
        path.push(currentRules.toSeq)
        findAcceptedPaths(path, paths)
        path.pop()
    paths

  def parseInput(input: String) =
    val inputRegex = """(?s)(.*)\R\R(.*)""".r
    val workflowRegex = """(\S+)\{(.*)}""".r
    val ruleRegex = """(?m)(\w+)(?:([<>])(\d+)\:(\w+)|$)""".r
    val partRegex = """(\d+)\D*(\d+)\D*(\d+)\D*(\d+)""".r

    val inputRegex(workflowLines, partLines) = input: @unchecked
    val workflows = (workflowLines.linesIterator
      .map: line =>
        val workflowRegex(wfName, wfParts) = line: @unchecked
        val rules = ruleRegex
          .findAllMatchIn(wfParts)
          .map: m =>
            val category = m.group(1)(0)
            val number = Option(m.group(3)).map(_.toInt).getOrElse(0)
            val target = Option(m.group(4)).getOrElse(m.group(1))
            val condition: Condition = m.group(2) match
              case ">" => part => part(category) > number
              case "<" => part => part(category) < number
              case _   => part => true
            Rule(condition, target, m.group(2), number, category)
          .toSeq
        (wfName -> Workflow(wfName, rules))
      .toSeq
      :+ ("A" -> Workflow("A")) :+ ("R" -> Workflow("R"))).toMap

    val categories = "xmas"
    val parts = partLines.linesIterator
      .map: line =>
        val m = partRegex.findFirstMatchIn(line).get
        m.subgroups.zipWithIndex.map((v, i) => categories(i) -> v.toInt).toMap
      .toSeq

    (workflows, parts)

  case class Rule(condition: Condition, target: String, op: String, number: Int, category: Char):
    def inverse: Rule = op match
      case "<" => Rule(null, target, ">", number - 1, category)
      case ">" => Rule(null, target, "<", number + 1, category)
      case _   => Rule(null, target, op, number, category)

  case class Workflow(name: String, rules: Seq[Rule] = Seq.empty):
    val isEnd = name == "R" || name == "A"
    val isAccepted = name == "A"

  type Part = Map[Char, Int]
  type Condition = (part: Part) => Boolean

  implicit class RangeOps(r1: Range):
    def containsRange(r2: Range): Boolean =
      if (r1.isInclusive || r2.isInclusive) throw new Exception("logic error")
      r1.contains(r2.start) || r1.contains(r2.end - 1)

end Day19
