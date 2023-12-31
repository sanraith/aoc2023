name := "aoc2023"
version := "1.0"
scalaVersion := "3.3.1"

// Run in "Forked JVM" to be cancelable without stopping sbt
run / fork := true
run / connectInput := true
run / javaOptions += "-Dfile.encoding=UTF-8"
outputStrategy := Some(StdoutOutput)

Compile / unmanagedJars += {
  baseDirectory.value / "unmanaged" / s"scalaz3_3-4.8.14.jar"
}

libraryDependencies ++= Seq(
  "org.jsoup" % "jsoup" % "1.16.2",
  "com.github.pureconfig" % "pureconfig_2.13" % "0.17.4",
  "org.scalatest" %% "scalatest-funspec" % "3.2.17" % "test",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)

// Test alias for each day: "test-1".."test-25"
(1 to 25).flatMap(day =>
  addCommandAlias(
    s"test-$day",
    s"testOnly hu.sanraith.aoc2023.solution.Day${"%02d".format(day)}Test"
  )
)
