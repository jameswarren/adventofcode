package jw.aoc2020

import jw.aoc2020.Day21._

trait TestDay21Example {
  val sampleInput: List[String] =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      |trh fvjkl sbzzf mxmxvkd (contains dairy)
      |sqjhc fvjkl (contains soy)
      |sqjhc mxmxvkd sbzzf (contains fish)
      |""".stripMargin.split("\n").toList
}

class TestDay21 extends TestScaffold with TestDay21Example {

  val items = sampleInput flatMap parseLine

  "the solution" should "parse an input line correctly" in {
    parseLine(sampleInput.head) should equal(Some((List("dairy", "fish"), Set("mxmxvkd", "kfcds", "sqjhc", "nhms"))))
  }

  it should "determine the contraints" in {
    constraintMap(items) should equal(
      Map("fish" -> Set("mxmxvkd", "sqjhc"), "soy" -> Set("sqjhc", "fvjkl"), "dairy" -> Set("mxmxvkd"))
    )
  }

  it should "identify the unique ingredient corresponding to an allergen" in {
    identifyIngredients(constraintMap(items)) should equal(
      Map("dairy" -> "mxmxvkd", "fish" -> "sqjhc", "soy" -> "fvjkl")
    )
  }
}

class TestDay21Part1 extends TestScaffold with TestDay21Example {

  "the solution" should "solve the example problem" in {
    Day21Part1.solve(sampleInput) should equal(5)
  }
}

class TestDay21Part2 extends TestScaffold with TestDay21Example {

  "the solution" should "solve the example problem" in {
    Day21Part2.solve(sampleInput) should equal("mxmxvkd,sqjhc,fvjkl")
  }
}
