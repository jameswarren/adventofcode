package jw.aoc2020

import jw.aoc2020.Day3._

import scala.io.Source

object Day3 {

  def isTree(template: String, pos: Int): Boolean = template(pos % template.length) == '#'

  def numCollisions(rows: Seq[String], step: Int): Int = {
    rows.zip(LazyList.from(0, step)).count {
      case (row, pos) => isTree(row, pos)
    }
  }
}

object Day3AltPart1 {

  def main(args: Array[String]): Unit = {
    val rows = Source.fromResource("aoc2020/day3.input").getLines().toList
    println(numCollisions(rows, 3))
  }
}

object Day3Part2 {

  def skipRows[A](rows: List[A], n: Int): List[A] = {
    rows.zipWithIndex.collect {
      case (r, idx) if idx % n == 0 => r
    }
  }

  def evaluateStrategy(rows: List[String], step: Int, skip: Int): Long = numCollisions(skipRows(rows, skip), step)

  def main(args: Array[String]): Unit = {
    val rows = Source.fromResource("aoc2020/day3.input").getLines().toList
    val strategies = List(1 -> 1, 3 -> 1, 5 -> 1, 7 -> 1, 1 -> 2)
    val product = strategies
      .map {
        case (step, skip) => evaluateStrategy(rows, step, skip)
      }
      .foldLeft(1L)(_ * _)

    println(product)
  }
}
