package jw.aoc2020

import jw.aoc2020.Day4._

class TestDay4 extends TestScaffold {

  "the parser" should "group adjacent, non-empty lines" in {
    val data = List("", "a", "b", "", "c", "d", "e", "", "f", "g", "", "")
    Utils.multiLineGroups(data).toList should equal(
      List(
        List("a", "b"),
        List("c", "d", "e"),
        List("f", "g")
      )
    )
  }

  it should "aggregate keys from identified events" in {
    val data = List("eyr:2030 hcl:#866857", "byr:1967 cid:316 pid:560346474 iyr:2015", "hgt:160cm", "ecl:gry")

    aggregate(data) should equal(
      Map(
        "eyr" -> "2030",
        "hcl" -> "#866857",
        "byr" -> "1967",
        "cid" -> "316",
        "pid" -> "560346474",
        "iyr" -> "2015",
        "hgt" -> "160cm",
        "ecl" -> "gry"
      )
    )
  }
}

class TestDay4Part2 extends TestScaffold {
  import Day4Part2._

  "the passport checker" should "verify year" in {
    val p = Passport(Map("byr" -> "1930"))
    p.checkYear("byr", 1920, 2000) shouldBe Some(true)
    p.checkYear("byr", 1940, 1960) shouldBe Some(false)
    p.checkYear("iyr", 1990, 2010) shouldBe None
  }

  it should "verify hair color" in {
    Passport(Map("hcl" -> "#00ffee")).checkHair shouldBe Some(true)
    Passport(Map("hcl" -> "00ffee")).checkHair shouldBe Some(false)
    Passport(Map("hcl" -> "#00ffeg")).checkHair shouldBe Some(false)
  }

  it should "verify eye color" in {
    Passport(Map("ecl" -> "blu")).checkEye shouldBe Some(true)
    Passport(Map("ecl" -> "blk")).checkEye shouldBe Some(false)
    Passport(Map.empty[String, String]).checkEye shouldBe None
  }

  it should "verify passport number" in {
    Passport(Map("pid" -> "012345678")).checkPassportId shouldBe Some(true)
    Passport(Map("pid" -> "0123456789")).checkPassportId shouldBe Some(false)
  }

  it should "verify height" in {
    Passport(Map("hgt" -> "140cm")).checkHeight shouldBe Some(false)
    Passport(Map("hgt" -> "160cm")).checkHeight shouldBe Some(true)
    Passport(Map("hgt" -> "45in")).checkHeight shouldBe Some(false)
    Passport(Map("hgt" -> "65in")).checkHeight shouldBe Some(true)
    Passport(Map("hgt" -> "120")).checkHeight shouldBe None
  }

  it should "return invalid if missing field" in {
    Passport(Map("byr" -> "1980")).isValid shouldBe false
  }
}
