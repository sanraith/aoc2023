package hu.sanraith.aoc2023.common

import scala.util.matching.Regex
import hu.sanraith.aoc2023.solution.Solution
import java.nio.file._
import java.nio.charset.StandardCharsets.UTF_8

object Util:
  val includesNewLineRegex: Regex = """^([\s\S]*\n[\s\S]*)$""".r
  val classDayRegex: Regex = """^.*?(\d+)$""".r
  val currentYear: Int = 2022

  def loadInputFromFile(solution: Solution) =
    val className = solution.getClass().getName()
    className match
      case classDayRegex(dayStr) => Files.readString(Paths.get("input", s"Day$dayStr.txt"), UTF_8)
      case _                     => throw Error(s"Cannot find puzzle input for class $className")
