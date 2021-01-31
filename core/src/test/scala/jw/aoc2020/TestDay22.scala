package jw.aoc2020

import jw.aoc2020.Day22._

class TestDay22 extends TestScaffold {

  val deck1 = List(9, 2, 6, 3, 1)
  val deck2 = List(5, 8, 4, 7, 10)

  "the solution" should "determine the initial deck from input" in {
    val input =
      """Player 1:
        |9
        |2
        |6
        |3
        |1""".stripMargin.split("\n").toList

    parseDeck(input) should equal(List(9, 2, 6, 3, 1))
  }
}

class TestDay22Part1 extends TestScaffold {
  import Day22Part1._

  val deck1 = List(9, 2, 6, 3, 1)
  val deck2 = List(5, 8, 4, 7, 10)
  val init: GameState = (deck1, deck2)

  it should "evaluate a round and update the decks by the winner" in {
    playRound(init) should equal((List(2, 6, 3, 1, 9, 5), List(8, 4, 7, 10)))
  }

  it should "simulate a game" in {
    playGame(init) should equal(List(3, 2, 10, 6, 8, 5, 9, 4, 7, 1))
  }

  it should "score the winning hand" in {
    scoreDeck(playGame(init)) should equal(306)
  }
}

class TestDay22Part2 extends TestScaffold {
  import Day22Part2._

  val deck1 = List(9, 2, 6, 3, 1)
  val deck2 = List(5, 8, 4, 7, 10)
  val init: GameState = (deck1, deck2)

  it should "simulate a recursive game" in {
    play(init, Set.empty) should equal((Nil, List(7, 5, 6, 2, 4, 1, 10, 8, 9, 3)))
  }

  it should "score the winning hand" in {
    scoreDeck(play(init, Set.empty)._2) should equal(291)
  }
}
