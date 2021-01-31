package jw.aoc2020

import jw.aoc2020.Day24._

import scala.io.Source

class TestDay24 extends TestScaffold {

  "the solution" should "parse a line into individual directions" in {
    parsePath("sesenwnenenewseeswwswswwnenewsewsw") should equal(
      Some(
        List(
          "se",
          "se",
          "nw",
          "ne",
          "ne",
          "ne",
          "w",
          "se",
          "e",
          "sw",
          "w",
          "sw",
          "sw",
          "w",
          "ne",
          "ne",
          "w",
          "se",
          "w",
          "sw"
        )
      )
    )
  }

  it should "traverse a given path" in {
    trace(List("nw", "w", "sw", "e", "e")) should equal(0 -> 0)
  }

}

class TestDay24Part1 extends TestScaffold {

  val input = Source.fromResource("./aoc2020/day24small.input").getLines().toList
  val paths = input flatMap parsePath

  "the solution" should "solve the example problem" in {
    Day24Part1.solve(paths) should equal(10)
  }
}

class TestDay24Part2 extends TestScaffold {

  val input = Source.fromResource("./aoc2020/day24small.input").getLines().toList
  val paths = input flatMap parsePath
  val start = destinations(paths)

  "the solution" should "determine the neighbors of a given cell" in {
    Day24Part2.neighbors((0, 0)) should equal(
      Set(
        2 -> 0,
        -2 -> 0,
        1 -> 1,
        1 -> -1,
        -1 -> 1,
        -1 -> -1
      )
    )
  }

  it should "replicate the iterative pattern from the example problem" in {
    Day24Part2.evolution(start).take(11).toList should equal(
      List(10, 15, 12, 25, 14, 23, 28, 41, 37, 49, 37)
    )
  }

  it should "solve the example problem" in {
    Day24Part2.solve(start) should equal(2208)
  }
}
