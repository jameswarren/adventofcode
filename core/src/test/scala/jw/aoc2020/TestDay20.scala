package jw.aoc2020

import cats.syntax.all._
import jw.aoc2020.Day20._

import scala.io.Source

class TestDay20 extends TestScaffold {

  val input: List[String] =
    """Tile 3583 SymE:
      |.##..#..#.
      |....##....
      |##..#..#..
      |.....#....
      |.#..#.....
      |#.#.......
      |#.....#..#
      |....#....#
      |...#.##.#.
      |.#....##.#
      |""".stripMargin.split("\n").toList

  "the solution" should "parse a tile from text input" in {
    Tile(input).get.toString should equal(input.mkString("\n"))
  }

  it should "generate all possible boundaries" in {
    val t = Tile(input).get
    val expected = List(".##..#..#.", ".#....##.#", "..#..##...", "......##.#")

    Tile.boundary(t) should contain theSameElementsAs (expected ++ expected.map(_.reverse))
  }

  val example: List[String] = Source.fromResource("./aoc2020/day20small.input").getLines().toList
  val tiles: List[Tile] = (Utils.multiLineGroups(example) flatMap Tile.apply).toList
  val connectionMap: Map[String, Set[Long]] = (tiles map Tile.boundaryMap).combineAll

  "given a set of tiles, the solution" should "construct a connection map from boundaries to tile ids" in {
    val partialExpected = List(
      ".#####..#." -> Set(2311, 1951),
      "#..##.#..." -> Set(2311, 3079),
      "..###.#.#." -> Set(1427, 2473),
      ".#..#.##.." -> Set(2311, 1427)
    )
    connectionMap should contain allElementsOf partialExpected
  }

  it should "determine the edges of a piece given the connection map" in {
    val tile = tiles.head
    Tile.edges(tile, connectionMap) should contain theSameElementsAs Set("###..###..", "..###..###")
  }

  it should "identify the corners" in {
    // corners have two edges with two reflections
    tiles
      .map(
        t => Tile.edges(t, connectionMap)
      )
      .count(_.size == 4) should equal(4)
  }

  it should "solve the first part of the sample problem" in {
    val side = 3
    val solution = PuzzleSolver.solve(tiles, side)
    val corners = List((0, 0), (0, 2), (2, 0), (2, 2))
    corners
      .map(
        c => solution(c).id
      )
      .product should equal(20899048083289L)
  }

  it should "solve the second part of th esample problem" in {
    val side = 3
    val solution = PuzzleSolver.solve(tiles, side)
    Day20Part2.solve(solution, side) should equal(273)
  }
}
