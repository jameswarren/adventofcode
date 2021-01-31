package jw.aoc2020

class TestDay18 extends TestScaffold {
  import Day18Part1._

  "the solution" should "evaluate expressions without applying order of operations" in {
    solve("3 + 8") should equal(Some(11))
    solve("2 + 2 * 2 + 2") should equal(Some(10))
  }

  it should "evaluate more complex expressions using parentheses" in {
    solve("(7)") should equal(Some(7))
    solve("(7+3)") should equal(Some(10))
    solve("3+(7*5)") should equal(Some(38))
    solve("3+(7*5)*2") should equal(Some(76))
    solve("(7 * 9 + 2 + (5 + 4)) + 5 + 9") should equal(Some(88))
    solve("7 + (3 + 6 + 8) + (5 * 9 + 7 + 2 + (2 * 9 + 8) + 2) * 9") should equal(Some(954))
  }
}

class TestDay18Part2 extends TestScaffold {
  import Day18Part2._

  "the solution" should "solve the example problems" in {
    solve("1 + 2 * 3 + 4 * 5 + 6") should equal(Some(231))
    solve("2 * 3 + (4 * 5)") should equal(Some(46))
    solve("5 + (8 * 3 + 9 + 3 * 4 * 3)") should equal(Some(1445))
    solve("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") should equal(Some(669060))
    solve("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") should equal(Some(23340))
  }
}
