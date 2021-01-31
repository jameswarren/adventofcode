package jw.aoc2020

import jw.aoc2020.Day22._

import scala.io.Source

object Day22 {

  type Deck = List[Int]
  type GameState = (Deck, Deck)

  def parseDeck(input: Seq[String]): Deck = input.tail.map(_.toInt).toList
  def scoreDeck(d: Deck): Int =
    d.reverse
      .zip(LazyList.from(1))
      .map(
        p => p._1 * p._2
      )
      .sum
}

object Day22Part1 {

  def playRound(s: GameState): GameState = s match {
    case (Nil, _) => s
    case (_, Nil) => s
    case (h1 :: t1, h2 :: t2) =>
      if (h1 > h2) (t1 ++ List(h1, h2), t2)
      else (t1, t2 ++ List(h2, h1))
  }

  def playGame(s: GameState): Deck = {
    LazyList
      .iterate(s)(playRound)
      .collectFirst {
        case (p1, p2) if p1.isEmpty || p2.isEmpty => p1 ++ p2
      }
      .get
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("./aoc2020/day22.input").getLines().toList
    val groups = Utils.multiLineGroups(input)
    val deck1 = parseDeck(groups.head)
    val deck2 = parseDeck(groups.tail.head)
    println(scoreDeck(playGame((deck1, deck2))))
  }
}

object Day22Part2 {

  //scalastyle:off cyclomatic.complexity
  def play(state: GameState, history: Set[GameState]): GameState = {
    if (history.contains(state)) (state._1, Nil)
    else {
      state match {
        case (_, Nil) => state
        case (Nil, _) => state
        case (h1 :: t1, h2 :: t2) if h1 <= t1.length && h2 <= t2.length =>
          play((t1.take(h1), t2.take(h2)), Set.empty) match {
            case (_, Nil) => play((t1 ++ List(h1, h2), t2), history + state)
            case (Nil, _) => play((t1, t2 ++ List(h2, h1)), history + state)
            case _        => throw new Exception("impossible state")
          }
        case (h1 :: t1, h2 :: t2) =>
          if (h1 > h2) play((t1 ++ List(h1, h2), t2), history + state)
          else play((t1, t2 ++ List(h2, h1)), history + state)
      }
    }
  }
  //scalastyle:on

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("./aoc2020/day22.input").getLines().toList
    val groups = Utils.multiLineGroups(input)
    val deck1 = parseDeck(groups.head)
    val deck2 = parseDeck(groups.tail.head)

    val (result1, result2) = play((deck1, deck2), Set.empty)
    println(scoreDeck(result1 ++ result2))
  }
}
