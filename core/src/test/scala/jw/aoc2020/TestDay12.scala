package jw.aoc2020

import jw.aoc2020.Day12._

trait Day12Example {

  val testInstructions = List(
    Forward -> 10,
    North -> 3,
    Forward -> 7,
    Right -> 90,
    Forward -> 11
  )
}

class TestDay12Part1 extends TestScaffold with Day12Example {
  import Day12Part1._

  "the solution" should "solve the first problem" in {
    val initState = Ship(0, 0, East)
    val finalState = testInstructions.foldLeft(initState) {
      case (ship, (command, amount)) => move(ship, command, amount)
    }

    distance(finalState) should equal(25)
  }
}

class TestDay12Part2 extends TestScaffold with Day12Example {
  import Day12Part2._

  "the solution" should "solve the second problem" in {
    val initState = Ship(Location(0, 0), Location(10, 1))
    val finalState = testInstructions.foldLeft(initState) {
      case (ship, (command, amount)) => move(ship, command, amount)
    }

    finalState.pos should equal(Location(214, -72))
    finalState.pos.dist should equal(286)
  }
}
