name := "aoc2023"
version := "1.0"
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.jsoup" % "jsoup" % "1.16.2",
  "com.github.pureconfig" % "pureconfig_2.13" % "0.17.4",
  "org.scalatest" %% "scalatest-funspec" % "3.2.17" % "test"
)

(1 to 25).flatMap(day =>
  addCommandAlias(
    s"test-$day",
    s"testOnly hu.sanraith.aoc2023.solution.Day${"%02d".format(day)}Test"
  )
)
