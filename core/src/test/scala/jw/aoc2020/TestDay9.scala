package jw.aoc2020

import jw.aoc2020.Day9._

class TestDay9Part1 extends TestScaffold {
  import Day9Part1._

  "the solution" should "find the first number that is not a sum of 2 digits in the encryption range" in {
    val preamble: List[Long] = List(35, 20, 15, 25, 47)
    val message: List[Long] = List(40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)

    val initialized = init(preamble)
    findFirstNonSum(message, initialized) should equal(Some(127))
  }
}

class TestDay9Part2 extends TestScaffold {
  import Day9Part2._

  "the solution" should "determine the subsequence that adds to the target" in {
    val input: List[Long] =
      List(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)
    findContinuousSubsequence(input.toVector, 127) should equal(Some(Vector(15, 25, 47, 40)))
  }
}

class TestDay9Part2Alt extends TestScaffold {
  import Day9Part2Alt._

  "the solution" should "determine the subsequence that adds to the target" in {
    val input: List[Long] =
      List(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)
    val solution = solve(input, 127)

    solution.map(candidate(_, 127)) should equal(Some(Vector(15, 25, 47, 40), 127))
  }
}
