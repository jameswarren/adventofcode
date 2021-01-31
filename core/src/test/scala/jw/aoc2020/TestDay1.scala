package jw.aoc2020

class TestDay1Part1 extends TestScaffold {
  "the solution" should "solve the example problem" in {
    Day1Part1.solve(List(1721, 979, 366, 299, 675, 1456)) should equal(Some(514579))
  }
}

class TestDay1Part2 extends TestScaffold {
  "the solution" should "solve the example problem" in {
    Day1Part2a.solve(List(1721, 979, 366, 299, 675, 1456)) should equal(Some(241861950))
  }
}
