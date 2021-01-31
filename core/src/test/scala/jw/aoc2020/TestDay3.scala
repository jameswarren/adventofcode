package jw.aoc2020

import jw.aoc2020.Day3._

class TestDay3 extends TestScaffold {

  "the solution" should "detect if a position is a tree" in {
    isTree("..##.......", 2) shouldBe true
    isTree("..##.......", 12) shouldBe false
    isTree("..##.......", 13) shouldBe true
  }
}

class TestDay3Part1 extends TestScaffold {

  "the solution" should "determine the number of collisions by stepping over each line" in {
    val input =
      """..##.......
        |#...#...#..
        |.#....#..#.
        |..#.#...#.#
        |.#...##..#.
        |..#.##.....
        |.#.#.#....#
        |.#........#
        |#.##...#...
        |#...##....#
        |.#..#...#.#""".stripMargin.split("\n").toList

    numCollisions(input, 1) should equal(2)
  }
}

class TestDay3Part2 extends TestScaffold {
  import Day3Part2._

  "the Part2 solution" should "skip the specified rows" in {
    skipRows(List("a", "b", "c", "d", "e"), 3) should equal(List("a", "d"))
  }

  it should "solve the example input" in {
    val exampleInput: List[String] =
      """..##.......
        |#...#...#..
        |.#....#..#.
        |..#.#...#.#
        |.#...##..#.
        |..#.##.....
        |.#.#.#....#
        |.#........#
        |#.##...#...
        |#...##....#
        |.#..#...#.#""".stripMargin.split("\n").toList

    evaluateStrategy(exampleInput, 1, 1) should equal(2)
    evaluateStrategy(exampleInput, 3, 1) should equal(7)
    evaluateStrategy(exampleInput, 5, 1) should equal(3)
    evaluateStrategy(exampleInput, 7, 1) should equal(4)
    evaluateStrategy(exampleInput, 1, 2) should equal(2)
  }
}
