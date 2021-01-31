package jw.aoc2020

trait Day11Example {
  val input: String =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL""".stripMargin

  val convergedPart1: String =
    """#.#L.L#.##
      |#LLL#LL.L#
      |L.#.L..#..
      |#L##.##.L#
      |#.#L.LL.LL
      |#.#L#L#.##
      |..L.L.....
      |#L#L##L#L#
      |#.LLLLLL.L
      |#.#L#L#.##""".stripMargin

  val convergedPart2: String =
    """#.L#.L#.L#
      |#LLLLLL.LL
      |L.L.L..#..
      |##L#.#L.L#
      |L.L#.LL.L#
      |#.LLLL#.LL
      |..#.L.....
      |LLL###LLL#
      |#.LLLLL#.L
      |#.L#LL#.L#
      |""".stripMargin

  val lines: List[String] = input.split("\n").toList
}

class TestDay11 extends TestScaffold with Day11Example {
  import Day11._

  "the solution" should "parse a grid" in {
    val grid = parseGrid(lines)
    gridToString(grid) should equal(input)
  }
}

class TestDay11Part1 extends TestScaffold with Day11Example {
  import Day11._
  import Day11Part1._

  "the solution" should "converge to known fixed point" in {
    val grid = parseGrid(lines)
    val converged = fixedPoint(grid)(evolve)
    converged.get should equal(parseGrid(convergedPart1.split("\n").toList))
  }
}

class TestDay11Part2 extends TestScaffold with Day11Example {
  import Day11._
  import Day11Part2._

  "the solution" should "converge to known fixed point" in {
    val grid = parseGrid(lines)
    val converged = fixedPoint(grid)(evolve)
    converged.get should equal(parseGrid(convergedPart2.split("\n").toList))
  }
}
