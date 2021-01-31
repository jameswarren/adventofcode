package jw.aoc2020

import jw.aoc2020.Day14._

import scala.io.Source

object Day14 {

  def valueFromBinaryString(s: String): Long = {
    s.foldLeft(0L) {
      case (agg, '1') => 2 * agg + 1
      case (agg, _)   => 2 * agg
    }
  }

  def extractBitandMask(s: String): String = s map {
    case '0' => '0'
    case _   => '1'
  }

  def extractBitorMask(s: String): String = s map {
    case '1' => '1'
    case _   => '0'
  }

  def applyMasks(n: Long, andMask: Long, orMask: Long): Long = (n & andMask) | orMask

  def parseAssignmentLine(line: String): Option[(Long, Long)] = {
    val pattern = "mem\\[(\\d+)\\] = (\\d+)".r
    pattern.findFirstMatchIn(line) map {
      m =>
        m.group(1).toLong -> m.group(2).toLong
    }
  }

  def extractGroups(lines: List[String]): LazyList[List[String]] = lines match {
    case Nil => LazyList.empty
    case h :: t =>
      val assignments = t.takeWhile(_.startsWith("mem"))
      (h :: assignments) #:: extractGroups(t.drop(assignments.length))
  }

  def aggregateGroups(groups: Seq[List[String]])(f: List[String] => Map[Long, Long]): Map[Long, Long] = {
    groups.map(f).foldLeft(Map.empty[Long, Long]) {
      case (agg, m) => agg ++ m
    }
  }
}

object Day14Part1 {

  def processGroup(group: List[String]): Map[Long, Long] = {
    val mask = group.head.drop(7)
    val bitAndMask = valueFromBinaryString(extractBitandMask(mask))
    val bitOrMask = valueFromBinaryString(extractBitorMask(mask))
    val assignments = group.tail flatMap parseAssignmentLine

    assignments.foldLeft(Map.empty[Long, Long]) {
      case (agg, (idx, n)) => agg + (idx -> applyMasks(n, bitAndMask, bitOrMask))
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day14.input").getLines().toList
    val aggregate = aggregateGroups(extractGroups(input))(processGroup)
    println(aggregate.values.sum)
  }
}

object Day14Part2 {

  def basicMask(s: String): Long = valueFromBinaryString(s.replaceAll("X", "0"))

  def floatingBits(s: String): List[Int] = s.reverse.zipWithIndex.filter(_._1 == 'X').map(_._2).toList

  def setBitToZero(n: Long, bit: Int): Long = n & ~(1L << bit)

  def setBitToOne(n: Long, bit: Int): Long = n | (1L << bit)

  def floatSequence(n: Long, floatingBits: List[Int]): LazyList[Long] = floatingBits match {
    case Nil => n #:: LazyList.empty
    case h :: t =>
      floatSequence(setBitToZero(n, h), t) ++ floatSequence(setBitToOne(n, h), t)
  }

  def processGroup(group: List[String]): Map[Long, Long] = {
    val mask = group.head.drop(7)
    val basic = basicMask(mask)
    val floating = floatingBits(mask)

    def processAssignment(a: (Long, Long)): Map[Long, Long] = {
      val base = a._1 | basic
      val locations = floatSequence(base, floating)
      locations.map((_, a._2)).toMap
    }

    val assignments = group.tail flatMap parseAssignmentLine
    assignments.map(processAssignment).foldLeft(Map.empty[Long, Long])(_ ++ _)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day14.input").getLines().toList
    val aggregate = aggregateGroups(extractGroups(input))(processGroup)
    println(aggregate.values.sum)
  }
}
