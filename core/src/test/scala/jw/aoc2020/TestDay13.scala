package jw.aoc2020

class TestDay13Part1 extends TestScaffold {
  import Day13Part1._

  "the solution" should "find the smallest multiple exceeding a threshold" in {
    nextArrival(939, 7) should equal(945)
  }

  it should "solve the example problem" in {
    nextBus(939, List(7, 13, 59, 31, 19)) should equal((59, 944))
  }
}

class TestDay13Part2 extends TestScaffold {
  import Day13Part2._

  val conds = List(
    Condition(7, 0),
    Condition(13, 1),
    Condition(59, 4),
    Condition(31, 6),
    Condition(19, 7)
  )

  "the solution" should "determine the conditions from the input" in {
    extractConditions("7,13,x,x,59,x,31,19") should equal(conds)
  }

  it should "solve the example problem" in {
    solve(conds) should equal(1068781)
  }

  it should "solve other examples" in {
    val testCases = List(
      "17,x,13,19" -> 3417,
      "67,7,59,61" -> 754018,
      "67,x,7,59,61" -> 779210,
      "67,7,x,59,61" -> 1261476,
      "1789,37,47,1889" -> 1202161486L
    )

    testCases foreach {
      case (input, ans) => solve(extractConditions(input)) should equal(ans)
    }
  }
}
