package jw.aoc2020

import jw.aoc2020.Day15._

//@org.scalatest.tags.Slow
class TestDay15 extends TestScaffold {

  it should "solve the given problems" in {
    solve(List(0, 3, 6), 2020) should equal(436)
    solve(List(1, 3, 2), 2020) should equal(1)
    solve(List(2, 1, 3), 2020) should equal(10)
    solve(List(1, 2, 3), 2020) should equal(27)
    solve(List(2, 3, 1), 2020) should equal(78)
    solve(List(3, 2, 1), 2020) should equal(438)
    solve(List(3, 1, 2), 2020) should equal(1836)

    solve(List(0, 3, 6), 30000000) should equal(175594)
    solve(List(1, 3, 2), 30000000) should equal(2578)
    solve(List(2, 1, 3), 30000000) should equal(3544142)
    solve(List(1, 2, 3), 30000000) should equal(261214)
    solve(List(2, 3, 1), 30000000) should equal(6895259)
    solve(List(3, 2, 1), 30000000) should equal(18)
    solve(List(3, 1, 2), 30000000) should equal(362)
  }
}
