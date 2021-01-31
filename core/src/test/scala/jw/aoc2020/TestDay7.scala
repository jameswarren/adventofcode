package jw.aoc2020

import jw.aoc2020.Day7._

class TestDay7 extends TestScaffold {

  "the solution" should "parse individual lines" in {
    val input1 = "plaid aqua bags contain no other bags."
    parse(input1) should equal(
      Some("plaid aqua" -> Map.empty[String, Int])
    )

    val input2 = "drab tan bags contain 5 plaid silver bags, 3 muted crimson bags, 1 clear salmon bag."
    parse(input2) should equal(
      Some("drab tan" -> Map("plaid silver" -> 5, "muted crimson" -> 3, "clear salmon" -> 1))
    )

    val input3 = "drab olive bags contain 1 shiny indigo bag."
    parse(input3) should equal(
      Some("drab olive" -> Map("shiny indigo" -> 1))
    )
  }
}
