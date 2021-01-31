package jw.aoc2020

import jw.aoc2020.Day2._

class TestDay2Part1 extends TestScaffold {
  import Day2Part1._

  "the solution" should "determine if # of the target character is within bounds" in {
    isValid(PasswordTest("abcde", 'a', 1, 3)) shouldBe true
    isValid(PasswordTest("cdefg", 'b', 1, 3)) shouldBe false
    isValid(PasswordTest("ccccccccc", 'c', 1, 9)) shouldBe true
  }

  it should "extract test parameters from a string" in {
    extractTest("1-3 a: abcde") should equal(Some(PasswordTest("abcde", 'a', 1, 3)))
  }
}

class TestDay2Part2 extends TestScaffold {
  import Day2Part2._

  "the solution" should "determine if the target character is present exactly once at given positions" in {
    isValid(PasswordTest("abcde", 'a', 1, 3)) shouldBe true
    isValid(PasswordTest("cdefg", 'b', 1, 3)) shouldBe false
    isValid(PasswordTest("ccccccccc", 'c', 1, 9)) shouldBe false
  }
}
