package jw.aoc2020

import jw.aoc2020.Day5._

import scala.io.Source

object Day5 {

  def convertFromBinary(s: String): Int = {
    def bit(c: Char): Int = c match {
      case 'B' | 'R' => 1
      case _         => 0
    }

    s.map(bit).foldLeft(0) {
      case (agg, b) => 2 * agg + b
    }
  }

  def seatId(s: String): Int = {
    val (rowChars, seatChars) = s.splitAt(7)
    8 * convertFromBinary(rowChars) + convertFromBinary(seatChars)
  }

}

object Day5Part1 {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day5.input").getLines()
    println(input.map(seatId).max)
  }
}

object Day5Part2 {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day5.input").getLines()
    val data = input.map(seatId).toList
    val minSeat = data.min
    val offset = minSeat - 1
    val length = data.length
    val normalizedTotal = data.map(_ - offset).sum
    val missing = (length + 1) * (length + 2) / 2 - normalizedTotal

    println(minSeat, length, missing + offset)
  }
}
