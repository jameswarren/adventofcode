package jw.aoc2020

class TestDay19 extends TestScaffold {
  import Day19Part1._

  "the solution" should "parse the given rules" in {
    parseRuleDescription("5: \"a\"") should equal(Some((5, Single("a"))))
    parseRuleDescription("7: 1") should equal(Some((7, Combination(List(List(1))))))
    parseRuleDescription("12: 5 15") should equal(Some((12, Combination(List(List(5, 15))))))
    parseRuleDescription("13: 4 8 | 6 9") should equal(Some((13, Combination(List(List(4, 8), List(6, 9))))))
  }

  it should "create a parser given rule descriptions" in {
    val ruleMap = Map(
      0 -> Combination(List(List(1, 2))),
      1 -> Single("a"),
      2 -> Combination(List(List(1, 3), List(3, 1))),
      3 -> Single("b")
    )

    val regexGen = getRegexGenerator(0).runA(ruleMap, Map.empty).value
    val matchFn = matcher(regexGen)

    matchFn("aab") shouldBe true
    matchFn("aba") shouldBe true
    matchFn("ab") shouldBe false
    matchFn("aaba") shouldBe false
  }

  it should "solve the second example" in {
    val ruleMap = Map(
      0 -> Combination(List(List(4, 1, 5))),
      1 -> Combination(List(List(2, 3), List(3, 2))),
      2 -> Combination(List(List(4, 4), List(5, 5))),
      3 -> Combination(List(List(4, 5), List(5, 4))),
      4 -> Single("a"),
      5 -> Single("b")
    )

    val regexGen = getRegexGenerator(0).runA(ruleMap, Map.empty).value
    val matchFn = matcher(regexGen)

    val testCases = List("ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb")
    testCases.count(matchFn) should equal(2)
  }
}
