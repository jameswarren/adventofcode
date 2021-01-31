package jw.aoc2020

import cats.Monoid
import cats.instances.set._

import scala.io.Source

object Day6Part1 {

  def assembleSet(group: List[String]): Set[Char] = {
    group.map(_.toSet).foldLeft(Set.empty[Char])(Monoid[Set[Char]].combine)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day6.input").getLines().toList
    val total = Utils.multiLineGroups(input).map(assembleSet).foldLeft(0) {
      case (agg, set) => agg + set.size
    }
    println(total)
  }
}

object Day6Part2 {

  case class Summary(size: Int, answers: Map[Char, Int]) {
    val allAnswered: Int = answers.count {
      case (_, n) => n == size
    }
  }

  def assembleSummary(group: List[String]): Summary = {
    val answers = Monoid[Map[Char, Int]].combineAll(for {
      line <- group
      c    <- line
    } yield Map(c -> 1))

    Summary(group.length, answers)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day6.input").getLines().toList
    val total = Utils.multiLineGroups(input).map(assembleSummary).foldLeft(0) {
      case (agg, summary) => agg + summary.allAnswered
    }
    println(total)
  }
}
