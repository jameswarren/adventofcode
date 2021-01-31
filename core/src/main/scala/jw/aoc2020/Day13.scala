package jw.aoc2020

import scala.io.Source
import scala.util.Try

object Day13Part1 {

  def nextArrival(target: Int, interval: Int): Int = {
    val quotient = target / interval
    (target % interval) match {
      case 0 => quotient * interval
      case _ => (quotient + 1) * interval
    }
  }

  def nextBus(target: Int, intervals: Seq[Int]): (Int, Int) = {
    intervals
      .map(
        i => i -> nextArrival(target, i)
      )
      .minBy(_._2)
  }

  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("aoc2020/day13.input").getLines().toList
    val target: Int = input.head.toInt
    val intervals: Array[Int] = input(1)
      .split(",")
      .flatMap(
        i => Try(i.toInt).toOption
      )
    val (bus, arrival) = nextBus(target, intervals.toIndexedSeq)
    println(bus * (arrival - target))
  }
}

object Day13Part2 {

  case class Sieve(start: Long, increment: Long)
  case class Condition(interval: Int, remainder: Int)

  def extractConditions(s: String): Seq[Condition] = {
    s.split(",").toIndexedSeq.zipWithIndex.flatMap {
      case (n, idx) => Try(Condition(n.toInt, idx)).toOption
    }
  }

  def constraint(c: Condition): Long => Boolean =
    l => (l + c.remainder) % c.interval == 0

  def search(s: Sieve, c: Condition): Sieve = {
    val stream = LazyList.iterate(s.start)(_ + s.increment)
    val nextStart = stream.filter(constraint(c)).head
    Sieve(nextStart, s.increment * c.interval)
  }

  def solve(cs: Seq[Condition]): Long = {
    cs.foldLeft(Sieve(1, 1)) {
      case (s, c) => search(s, c)
    }.start
  }

  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromResource("aoc2020/day13.input").getLines().toList
    val conditions = extractConditions(input(1))
    println(solve(conditions))
  }
}
