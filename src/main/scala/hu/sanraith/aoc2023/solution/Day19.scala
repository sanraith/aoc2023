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
      .filter(part => pathRanges.exists(rs => rs.forall((c, r) => r.contains(part(c)))))
      .map(p => p.values.sum)
      .sum

  override def part2(ctx: Context): Long =
    val (workflows, parts) = parseInput(ctx.input)
    val rangesByPath = getPathRanges(workflows)
    val possRanges = {
      val rangeSets = "xmas".map(c => c -> mut.Set.empty[Int]).toMap
      rangesByPath.foreach(cm => cm.foreach((c, r) => rangeSets(c).addAll(Seq(r.start, r.end))))
      rangeSets.map: (c, set) =>
        c -> set.toSeq.sorted
          .sliding(2)
          .map { case Seq(a, b) => a until b }
          .filter(r => rangesByPath.exists(_(c).containsRange(r)))
          .toVector
    }

    var rxi = 0
    val rxCount = possRanges('x').length.toDouble
    val rangeSizes = possRanges('x').par.map: rx =>
      synchronized:
        ctx.progress(rxi / rxCount)
        rxi += 1
      var partialSum = 0L
      val xPaths = rangesByPath.filter(_('x').containsRange(rx))
      for (rm <- possRanges('m'))
        val mPaths = xPaths.filter(_('m').containsRange(rm))
        for (ra <- possRanges('a'))
          val aPaths = mPaths.filter(_('a').containsRange(ra))
          for (rs <- possRanges('s'))
            if (aPaths.exists(_('s').containsRange(rs)))
              partialSum += rx.length.longValue * rm.length * ra.length * rs.length
      partialSum
    rangeSizes.sum

  def getPathRanges(wfs: Map[String, Workflow]) =
    given workflows: Map[String, Workflow] = wfs
    val startRule = Rule('x', '#', 0, "in")
    val paths = findAcceptedPaths(mut.Stack(Seq(startRule)))
    paths.toSeq
      .map: path =>
        val ranges = mut.Map("xmas".map(c => c -> (1 until 4001)): _*)
        for (rules <- path)
          val appliedRules = rules.take(rules.length - 1).map(_.inverse) :+ rules.last
          for (rule <- appliedRules)
            lazy val r = ranges(rule.category)
            rule.op match
              case '>' => ranges.addOne((rule.category, rule.number + 1 until r.end))
              case '<' => ranges.addOne((rule.category, r.start until rule.number))
              case _   => ()
        ranges.toMap
      .filter(ranges => ranges.values.forall(r => r.start <= r.end))
      .toVector

  def findAcceptedPaths(
      path: mut.Stack[Seq[Rule]],
      paths: mut.ArrayBuffer[Seq[Seq[Rule]]] = mut.ArrayBuffer.empty
  )(using workflows: Map[String, Workflow]): Iterable[Seq[Seq[Rule]]] =
    val wf = workflows(path.top.last.target)
    if (!wf.isEnd)
      val currentRules = mut.ArrayBuffer.empty[Rule]
      for (next <- wf.rules)
        currentRules.addOne(next)
        path.push(currentRules.toSeq)
        findAcceptedPaths(path, paths)
        path.pop()
    else if (wf.isAccepted)
      paths.addOne(path.toSeq.reverse)
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
            val op = Option(m.group(2)).map(_(0)).getOrElse('#')
            val number = Option(m.group(3)).map(_.toInt).getOrElse(0)
            val target = Option(m.group(4)).getOrElse(m.group(1))
            Rule(category, op, number, target)
          .toSeq
        Workflow(wfName, rules)
      .toSeq :+ Workflow("A") :+ Workflow("R")).map(wf => wf.name -> wf).toMap

    val categories = "xmas"
    val parts = partLines.linesIterator
      .map: line =>
        val mtch = partRegex.findFirstMatchIn(line).get
        mtch.subgroups.zipWithIndex.map((v, i) => categories(i) -> v.toInt).toMap
      .toSeq

    (workflows, parts)

  case class Rule(category: Char, op: Char, number: Int, target: String):
    def inverse: Rule = op match
      case '<' => Rule(category, '>', number - 1, target)
      case '>' => Rule(category, '<', number + 1, target)
      case _   => Rule(category, op, number, target)

  case class Workflow(name: String, rules: Seq[Rule] = Seq.empty):
    val isEnd = name == "R" || name == "A"
    val isAccepted = name == "A"

  implicit class RangeOps(r1: Range):
    def containsRange(r2: Range): Boolean =
      if (r1.isInclusive || r2.isInclusive) throw new Exception("logic error")
      r1.contains(r2.start) || r1.contains(r2.end - 1)

end Day19
