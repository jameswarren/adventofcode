package jw.aoc2020

import jw.aoc2020.Day16._

trait Day16Example {

  val exampleProblem: List[String] =
    """class: 1-3 or 5-7
      |row: 6-11 or 33-44
      |seat: 13-40 or 45-50
      |
      |your ticket:
      |7,1,14
      |
      |nearby tickets:
      |7,3,47
      |40,4,50
      |55,2,20
      |38,6,12
      |""".stripMargin.split("\n").toList
}

class TestDay16 extends TestScaffold with Day16Example {

  "the solution" should "parse a line describing a rule" in {
    val input = "departure location: 45-422 or 444-950"
    parseRule(input) should equal(Some(Rule("departure location", List(Range(45, 422), Range(444, 950)))))
  }

  it should "determine if a number is invalid according to rules" in {
    val ruleDescriptions =
      """class: 1-3 or 5-7
        |row: 6-11 or 33-44
        |seat: 13-40 or 45-50
        |""".stripMargin
    val rules = ruleDescriptions.split("\n").toList flatMap parseRule

    findInvalidField(List(7, 3, 47), rules) should equal(None)
    findInvalidField(List(40, 4, 50), rules) should equal(Some(4))
    findInvalidField(List(55, 2, 20), rules) should equal(Some(55))
    findInvalidField(List(38, 6, 12), rules) should equal(Some(12))
  }

  it should "determine sections in given input" in {
    sectionInput(exampleProblem) should equal(
      (
        List("class: 1-3 or 5-7", "row: 6-11 or 33-44", "seat: 13-40 or 45-50"),
        "7,1,14",
        List("7,3,47", "40,4,50", "55,2,20", "38,6,12")
      )
    )
  }
}

class TestDay16Part1 extends TestScaffold with Day16Example {

  "the solution" should "solve the example problem" in {
    Day16Part1.solve(exampleProblem) should equal(71)
  }
}

class TestDay16Part2 extends TestScaffold with Day16Example {
  import Day16Part2._

  it should "aggregate ticket data by position" in {
    val tickets = List(
      List(1, 12, 23),
      List(4, 15, 26),
      List(7, 18, 29)
    )

    aggregateByPosition(tickets) should equal(
      Map(
        0 -> List(1, 4, 7),
        1 -> List(12, 15, 18),
        2 -> List(23, 26, 29)
      )
    )
  }

  it should "determine a mapping of rules to data that is consistent" in {
    val rules = List(
      Rule("rule1", List(Range(1, 20))),
      Rule("rule2", List(Range(11, 20))),
      Rule("rule3", List(Range(15, 25))),
      Rule("rule4", List(Range(30, 40)))
    )

    val groupings = Map(
      0 -> List(31, 39, 38),
      1 -> List(9, 5, 1),
      2 -> List(17, 19, 18),
      3 -> List(13, 15, 17)
    )

    mapRulesToGroups(rules, groupings).get should contain theSameElementsAs (List(
      Rule("rule1", List(Range(1, 20))) -> 1,
      Rule("rule2", List(Range(11, 20))) -> 3,
      Rule("rule3", List(Range(15, 25))) -> 2,
      Rule("rule4", List(Range(30, 40))) -> 0
    ))
  }

  it should "correctly score a ticket given a rule map" in {
    val ruleMap = Map(
      Rule("departure 1", Nil) -> 2,
      Rule("arrival 2", Nil) -> 4,
      Rule("departure 5", Nil) -> 5
    )
    val ticket = List(0, 10, 20, 30, 40, 50, 60)

    scoreTicket(ruleMap, ticket) should equal(1000)
  }

}
