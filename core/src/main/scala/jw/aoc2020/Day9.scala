package jw.aoc2020

import cats.syntax.all._
import jw.aoc2020.Day9._

import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  case class State(cache: Vector[Long], sums: Map[Long, Int])

  def init(preamble: Seq[Long]): State = {
    val sums = countMap(pairwiseSums(preamble))
    State(preamble.toVector, sums)
  }

  def addValue(s: State, v: Long): State = {
    val removal = countMap(s.cache.tail.map(_ + s.cache.head), -1)
    val addition = countMap(s.cache.tail.map(_ + v))
    val updated = (s.sums |+| removal |+| addition).filter(_._2 != 0)
    State(s.cache.tail :+ v, updated)
  }

  def pairwiseSums(vals: Seq[Long]): Seq[Long] = {
    (for {
      current <- vals.tails
      head    <- current.headOption.toList
      t       <- current.tail
    } yield head + t).to(LazyList)
  }

  def countMap(vals: Seq[Long], increment: Int = 1): Map[Long, Int] = {
    vals.groupMapReduce(identity)(
      _ => increment
    )(_ + _)
  }

  val input: List[Long] = Source.fromResource("aoc2020/day9.input").getLines().toList.map(_.toLong)
}

object Day9Part1 {

  @tailrec
  def findFirstNonSum(xs: List[Long], s: State): Option[Long] = xs match {
    case Nil                           => None
    case h :: _ if !s.sums.contains(h) => Some(h)
    case h :: t                        => findFirstNonSum(t, addValue(s, h))
  }

  def part1Solution: Option[Long] = {
    val (preamble, message) = input.splitAt(25)
    val initialized = init(preamble)
    findFirstNonSum(message, initialized)
  }

  def main(args: Array[String]): Unit = println(part1Solution)
}

object Day9Part2 {

  def findContinuousSubsequence(vals: Vector[Long], target: Long): Option[Vector[Long]] = {
    @tailrec
    def loop(pos: Int, start: Int, sum: Long): Option[Vector[Long]] = {
      if (sum == target) Some(vals.slice(start, pos))
      else if (pos == vals.length) None
      else if (sum > target) loop(pos, start + 1, sum - vals(start))
      else /* sum < target */ loop(pos + 1, start, sum + vals(pos))
    }

    loop(0, 0, 0)
  }

  def main(args: Array[String]): Unit = {
    for {
      target <- Day9Part1.part1Solution
      subseq <- findContinuousSubsequence(input.toVector, target)
    } yield {
      println(subseq.min + subseq.max)
    }
  }
}

object Day9Part2Alt {

  def candidate(vals: List[Long], target: Long): (Vector[Long], Long) = {
    @tailrec
    def loop(vs: List[Long], prefix: Vector[Long], sum: Long): (Vector[Long], Long) = (vs, sum) match {
      case (_, n) if n >= target => (prefix, n)
      case (Nil, _)              => (prefix, sum)
      case (h :: t, n)           => loop(t, prefix :+ h, n + h)
    }
    loop(vals, Vector.empty[Long], 0)
  }

  def solve(vals: List[Long], target: Long): Option[List[Long]] = {
    vals
      .tails
      .find(
        vs => candidate(vs, target)._2 == target
      )
  }

  def main(args: Array[String]): Unit = {
    for {
      target   <- Day9Part1.part1Solution
      solution <- solve(input, target)
      (r, _) = candidate(solution, target)
    } yield println(r.min + r.max)
  }
}
