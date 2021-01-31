package jw.aoc2020

import atto.Atto._
import atto._
import cats.syntax.all._
import jw.aoc2020.Day24._

import scala.io.Source

object Day24 {

  type Coords = (Int, Int) // xy coordinates

  def parsePath(s: String): Option[List[String]] = {
    val directionParser = string("ne") | string("nw") | string("se") | string("sw") | string("e") | string("w")
    many(directionParser).parse(s).done.option
  }

  val offsets: Map[String, Coords] = Map(
    "se" -> (1, -1),
    "sw" -> (-1, -1),
    "ne" -> (1, 1),
    "nw" -> (-1, 1),
    "e" -> (2, 0),
    "w" -> (-2, 0)
  )

  def trace(directions: List[String]): Coords = directions.map(offsets).combineAll

  def destinations(paths: List[List[String]]): Set[Coords] = {
    val visits = paths
      .map(trace)
      .groupMapReduce(identity)(
        _ => 1
      )(_ + _)
    val blackTiles = visits.toList.collect {
      case (tile, count) if count % 2 == 1 => tile
    }
    blackTiles.toSet
  }
}

object Day24Part1 extends App {

  def solve(paths: List[List[String]]): Int = destinations(paths).size

  val input = Source.fromResource("./aoc2020/day24.input").getLines().toList
  val paths = input flatMap parsePath
  println(solve(paths))
}

object Day24Part2 extends App {

  def neighbors(c: Coords): Set[Coords] = offsets.toList.map(_._2 |+| c).toSet

  def tileStep(c: Coords, current: Set[Coords]): Option[Coords] = {
    val numBlackNeighbors = neighbors(c).intersect(current).size
    (current.contains(c), numBlackNeighbors) match {
      case (true, 1) | (true, 2) | (false, 2) => Some(c)
      case _                                  => None
    }
  }

  def step(current: Set[Coords]): Set[Coords] = {
    val candidates = current.flatMap(neighbors) ++ current
    candidates.flatMap(tileStep(_, current))
  }

  def evolution(start: Set[Coords]): LazyList[Int] = {
    LazyList.iterate(start)(step).map(_.size)
  }

  def solve(start: Set[Coords]): Int = evolution(start)(100)

  val input = Source.fromResource("./aoc2020/day24.input").getLines().toList
  val paths = input flatMap parsePath
  val start = destinations(paths)
  println(solve(start))
}
