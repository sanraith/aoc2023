package hu.sanraith.aoc2023.solution

import scala.collection.{mutable => mut}
import scala.compiletime.ops.boolean

/** Solution for https://adventofcode.com/2023/day/20 */
class Day20 extends Solution:
  override val title: String = "Pulse Propagation"

  override def part1(ctx: Context): Long =
    val modules = parseInput(ctx.input)
    val broadcaster = modules("broadcaster")

    var (lowCount, highCount) = (0L, 0L)
    for (i <- 1 to 1000)
      val (low, high) = countPulses(broadcaster, modules)
      lowCount += low
      highCount += high
    lowCount * highCount

  // Solution specialized for the puzzle input where 4 counters are connected to the conjuction before rx.
  override def part2(ctx: Context): Long =
    val modules = parseInput(ctx.input)
    val broadcaster = modules("broadcaster")
    val rxInput = modules.values.find(_.targets.contains("rx")).get.asInstanceOf[Conjuction]
    val inputCycles = mut.Map.from(rxInput.states.keys.map(k => k -> -1L))

    var pressCount = 0
    while (inputCycles.values.exists(_ == -1))
      pressCount += 1
      countPulses(broadcaster, modules)
      rxInput.highInputs.filter(inputCycles(_) == -1).foreach(inputCycles(_) = pressCount)
    inputCycles.values.foldLeft(1L)(lcm)

  def lcm(a: Long, b: Long): Long =
    (a * b) / gcd(a, b)

  def gcd(a: Long, b: Long): Long =
    if (b == 0) a else gcd(b, a % b)

  def countPulses(
      startModule: Module,
      modules: Map[String, Module]
  ): (Int, Int) =
    val queue = mut.Queue(("button", false, startModule))
    var (lowCount, highCount) = (0, 0)
    while (queue.length > 0)
      val (from, signal, module) = queue.dequeue()
      signal match
        case true  => highCount += 1
        case false => lowCount += 1
      module.run(from, signal) match
        case Some(nextSignal) =>
          module.targets.foreach: target =>
            queue.enqueue((module.name, nextSignal, modules(target)))
        case None => ()
    (lowCount, highCount)

  def parseInput(inputs: String) =
    val lineRegex = """([&%]|)(\w+) -> (.+)""".r
    val targetRegex = """\w+""".r
    val modules = inputs.linesIterator
      .map:
        case lineRegex(op, name, targetsStr) =>
          val targets = targetRegex.findAllIn(targetsStr).toSeq
          op match
            case "%" => new FlipFlop(name, targets)
            case "&" => new Conjuction(name, targets)
            case ""  => new Broadcaster(name, targets)
      .map(m => m.name -> m)
      .toMap

    modules.values.collect { case m: Conjuction =>
      m.initialize(modules.values.filter(o => o.targets.contains(m.name)).map(_.name).toSeq)
    }
    modules ++ modules.values
      .flatMap(_.targets)
      .filter(!modules.contains(_))
      .map(n => n -> new OutModule(n))

  abstract class Module(val name: String, val targets: Seq[String]):
    def run(from: String, signal: Boolean): Option[Boolean]

  class OutModule(name: String) extends Module(name, Seq.empty):
    override def run(from: String, signal: Boolean) = None

  class Broadcaster(name: String, targets: Seq[String]) extends Module(name, targets):
    override def run(from: String, signal: Boolean) = Some(signal)

  class FlipFlop(name: String, targets: Seq[String]) extends Module(name, targets):
    var state = false
    override def run(from: String, signal: Boolean) =
      signal match
        case true => None
        case false =>
          state = !state
          Some(state)

  class Conjuction(name: String, targets: Seq[String]) extends Module(name, targets):
    val states = mut.Map.empty[String, Boolean]
    val highInputs = mut.Set.empty[String]

    def initialize(inputs: Iterable[String]) =
      inputs.foreach(states.addOne(_, false))

    override def run(from: String, signal: Boolean) =
      if (signal)
        highInputs.addOne(from)
      states.addOne(from, signal)
      Some(!states.values.forall(x => x))
