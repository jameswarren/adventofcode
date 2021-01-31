package jw.aoc2020

class TestDay6 extends TestScaffold {

  "the solution" should "group adjacent non-empty lines" in {
    val input = List("", "a", "b", "", "", "c", "d", "", "e", "")
    Utils.multiLineGroups(input) should equal(
      List(
        List("a", "b"),
        List("c", "d"),
        List("e")
      )
    )
  }
}

class TestDay6Part1 extends TestScaffold {
  import Day6Part1._

  "the solution" should "assemble answers in groups" in {
    val input = List("abc", "bcd", "cde")
    assembleSet(input) should equal(Set('a', 'b', 'c', 'd', 'e'))
  }
}

class TestDay6Part2 extends TestScaffold {
  import Day6Part2._

  "the solution" should "summarize the answers of a group" in {
    val input = List("abc", "ab", "a")
    val summary = assembleSummary(input)
    summary.size should equal(3)
    summary.answers should equal(Map('a' -> 3, 'b' -> 2, 'c' -> 1))
    summary.allAnswered should equal(1)
  }
}
