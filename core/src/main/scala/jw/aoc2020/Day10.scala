package jw.aoc2020

import scala.io.Source

object Day10Part1 {

  def distribution(joltages: Seq[Int]): Map[Int, Int] = {
    val sorted = joltages.sorted
    val full: Seq[Int] = 0 +: sorted :+ (sorted.max + 3)
    val pairs = full.zip(full.tail)
    pairs.groupMapReduce {
      case (a, b) => b - a
    }(
      _ => 1
    )(_ + _)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day10.input").getLines().toList.map(_.toInt)
    val d = distribution(input)
    println(d(1) * d(3))
  }
}

object Day10Part2 {

  def countPaths(joltages: Seq[Int]): Long = {
    val sorted = joltages.sorted

    def count(m: Map[Int, Long], j: Int): Map[Int, Long] = {
      val numPaths = m.getOrElse(j - 3, 0L) + m.getOrElse(j - 2, 0L) + m.getOrElse(j - 1, 0L)
      m + (j -> numPaths)
    }

    val pathCounts = sorted.foldLeft(Map(0 -> 1L)) {
      case (m, j) => count(m, j)
    }

    pathCounts(sorted.max)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day10.input").getLines().toList.map(_.toInt)
    println(countPaths(input))
  }
}
