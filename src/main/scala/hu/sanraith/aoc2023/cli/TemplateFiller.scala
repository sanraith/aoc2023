package hu.sanraith.aoc2023.cli

import scala.util.matching.Regex
import java.util.regex.Matcher

class TemplateFiller(template: String):

  /** Replaces the placeholder with the given content. */
  def fill(placeholder: String, content: String): TemplateFiller =
    TemplateFiller(template.replaceAll(placeholder, Matcher.quoteReplacement(content)))

  /** Replaces the placeholder with the given lines, keeping the placeholders original indentation
    * level for each line.
    */
  def fill(
      placeholder: String,
      lines: Iterable[String],
      lineSeparator: String = "\n"
  ): TemplateFiller =
    val withIndentRegex = Regex("""(?m)^(\s*)""" + Regex.quote(placeholder))
    withIndentRegex.findFirstMatchIn(template) match
      case Some(m) =>
        val indent = m.group(1)
        val linesStr = Matcher.quoteReplacement(indent + lines.mkString(s"$lineSeparator$indent"))
        TemplateFiller(withIndentRegex.replaceFirstIn(template, linesStr))
      case None => this

  override def toString(): String = template
