package jw.aoc2020

class TestDay10Part1 extends TestScaffold {
  import Day10Part1._

  "the solution" should "determine the joltage gaps" in {
    val input = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
    distribution(input) should equal(Map(1 -> 7, 3 -> 5))
  }
}

class TestDay10Part2 extends TestScaffold {
  import Day10Part2._

  "the solution" should "count the number of paths" in {
    val input1 = List(1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19)
    countPaths(input1) should equal(8)

    val input2 = List(1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31, 32, 33, 34, 35, 38, 39, 42,
      45, 46, 47, 48, 49)
    countPaths(input2) should equal(19208)
  }
}
