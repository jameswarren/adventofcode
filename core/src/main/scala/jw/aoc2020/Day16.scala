package jw.aoc2020

import jw.aoc2020.Day16._

import scala.io.Source

object Day16 {

  case class Range(lower: Int, upper: Int) {
    def contains(n: Int): Boolean = lower <= n && n <= upper
  }

  case class Rule(name: String, ranges: List[Range]) {
    def isConsistent(n: Int): Boolean = ranges.exists(_.contains(n))
  }

  def parseRule(s: String): Option[Rule] = {
    val pattern = "(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
    pattern.findFirstMatchIn(s) map {
      m =>
        val range1 = Range(m.group(2).toInt, m.group(3).toInt)
        val range2 = Range(m.group(4).toInt, m.group(5).toInt)
        Rule(m.group(1), List(range1, range2))
    }
  }

  def findInvalidField(xs: Seq[Int], rules: Seq[Rule]): Option[Int] = xs.find {
    x =>
      rules.forall(!_.isConsistent(x))
  }

  def sectionInput(lines: Seq[String]): (Seq[String], String, Seq[String]) = {
    val ruleDescriptions = lines.takeWhile(_.nonEmpty)
    val ticketLine = lines.drop(ruleDescriptions.length + 2).head
    val otherTicketLines = lines.drop(ruleDescriptions.length + 5)
    (ruleDescriptions, ticketLine, otherTicketLines)
  }

  def parseTicket(s: String): Seq[Int] = s.split(",").toIndexedSeq.map(_.toInt)
}

object Day16Part1 {

  def solve(lines: Seq[String]): Int = {
    val (ruleDescriptions, _, otherTicketLines) = sectionInput(lines)
    val rules = ruleDescriptions flatMap parseRule
    val otherTickets = otherTicketLines map parseTicket

    otherTickets.flatMap(findInvalidField(_, rules)).sum
  }

  def main(args: Array[String]): Unit = {
    val problem = Source.fromResource("aoc2020/day16.input").getLines().toList
    println(solve(problem))
  }
}

object Day16Part2 {

  def aggregateByPosition(tickets: Seq[Seq[Int]]): Map[Int, Seq[Int]] = {
    val positions: Seq[(Int, Int)] = for {
      ticket <- tickets
      indexed = ticket.zipWithIndex
      (v, idx) <- indexed
    } yield (idx, v)

    positions.groupBy(_._1).map {
      case (k, pairs) => k -> pairs.map(_._2)
    }
  }

  def potentialMappings(rule: Rule, groups: Map[Int, Seq[Int]]): LazyList[Int] =
    groups
      .flatMap {
        case (idx, vals) =>
          if (vals.forall(rule.isConsistent)) Some(idx)
          else None
      }
      .toList
      .sorted
      .to(LazyList)

  def mapRulesToGroups(allRules: List[Rule], allGroups: Map[Int, Seq[Int]]): Option[List[(Rule, Int)]] = {

    def dfs(assignment: List[(Rule, Int)], rules: List[Rule], groups: Map[Int, Seq[Int]]): Option[List[(Rule, Int)]] = {
      rules match {
        case Nil => Some(assignment)
        case nextRule :: remainingRules =>
          potentialMappings(nextRule, groups)
            .map {
              c =>
                dfs(assignment :+ (nextRule -> c), remainingRules, groups - c)
            }
            .collectFirst {
              case Some(m) => m
            }
      }
    }

    dfs(Nil, allRules, allGroups)
  }

  def scoreTicket(ruleMap: Map[Rule, Int], ticket: Seq[Int]): Long = {
    ruleMap
      .flatMap {
        case (Rule(name, _), idx) if name.startsWith("departure") => Some(ticket(idx))
        case _                                                    => None
      }
      .foldLeft(1L)(_ * _)
  }

  def solve(lines: Seq[String]): Option[Long] = {
    val (ruleDescriptions, myTicketLine, otherTicketLines) = sectionInput(lines)
    val rules = ruleDescriptions flatMap parseRule
    val myTicket = parseTicket(myTicketLine)
    val otherTickets = otherTicketLines map parseTicket
    val validTickets = otherTickets.filter(
      t => findInvalidField(t, rules).isEmpty
    )
    val groups = aggregateByPosition(validTickets)
    val sortedRules = rules
      .toList
      .sortBy(
        r => potentialMappings(r, groups).length
      )

    for {
      ruleMap <- mapRulesToGroups(sortedRules, groups)
    } yield scoreTicket(ruleMap.toMap, myTicket)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("aoc2020/day16.input").getLines().toList
    println(solve(lines))
  }
}
