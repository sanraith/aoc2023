package hu.sanraith.aoc2023.util

implicit class IteratorExtensions[A](val it: Iterator[A]) extends AnyVal:
  def tapEachWithIndex(f: (item: A, index: Int) => Unit): Iterator[A] =
    it.zipWithIndex
      .tapEach((x, i) => f(x, i))
      .map((x, _) => x)
