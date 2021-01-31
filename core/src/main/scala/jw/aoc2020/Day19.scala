package jw.aoc2020

import atto.Atto._
import atto._
import cats.data.ReaderWriterState
import cats.syntax.all._

import scala.io.Source

trait Day19 {

  sealed trait RuleDescription
  case class Single(s: String) extends RuleDescription
  case class Combination(groups: List[List[Int]]) extends RuleDescription

  val parser: Parser[(Int, RuleDescription)] = {
    lazy val singleParser: Parser[RuleDescription] = {
      val base = char('"') ~> letter <~ char('"')
      base map (
        c => Single(c.toString)
      )
    }

    lazy val comboParser: Parser[RuleDescription] = {
      val groupParser = int.sepBy(spaceChar)
      groupParser sepBy string(" | ") map Combination.apply
    }

    for {
      n    <- int <~ string(": ")
      rule <- singleParser | comboParser
    } yield (n, rule)
  }

  def parseRuleDescription(s: String): Option[(Int, RuleDescription)] = parser.parse(s).done.option

  type RWState[A] = ReaderWriterState[RuleMap, List[String], ParserCache, A]
  type RuleMap = Map[Int, RuleDescription]
  type ParserCache = Map[Int, String]

  def getRegexGenerator(id: Int): RWState[String]

  def singleRegexGenerator(id: Int, s: String): RWState[String] = addRegexGeneratorToCache(id, s)

  def combinationRegexGenerator(id: Int, groups: List[List[Int]]): RWState[String] = {
    (groups traverse groupGenerator) map (_.reduce(disjunction)) flatMap {
      p =>
        addRegexGeneratorToCache(id, p)
    }
  }

  def groupGenerator(group: List[Int]): RWState[String] = (group traverse getRegexGenerator) map (_.reduce(conjunction))

  def addRegexGeneratorToCache(id: Int, p: String): RWState[String] =
    ReaderWriterState(
      (_, cache) => (List(s"adding $id to cache"), cache + (id -> p), p)
    )

  def disjunction(s1: String, s2: String): String = s"($s1)|($s2)"

  def conjunction(s1: String, s2: String): String = s"($s1)($s2)"

  def matcher(gen: String): String => Boolean = s => ("^" + gen + "$").r.matches(s)

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("./aoc2020/day19.input").getLines().toList
    val specs = input.takeWhile(_.nonEmpty)
    val messages = input.dropWhile(_.nonEmpty).dropWhile(_.isEmpty)
    val ruleMap = specs.flatMap(parseRuleDescription).toMap
    val regexGen = getRegexGenerator(0).runA(ruleMap, Map.empty).value

    println(messages.count(matcher(regexGen)))
  }
}

object Day19Part1 extends Day19 {

  override def getRegexGenerator(id: Int): RWState[String] = ReaderWriterState {
    (ruleMap, cache) =>
      if (cache.contains(id)) (List(s"found $id in cache"), cache, cache(id))
      else {
        ruleMap(id) match {
          case Single(s)           => singleRegexGenerator(id, s).run(ruleMap, cache).value
          case Combination(groups) => combinationRegexGenerator(id, groups).run(ruleMap, cache).value
        }
      }
  }
}

object Day19Part2 extends Day19 {

  override def getRegexGenerator(id: Int): RWState[String] = ReaderWriterState {
    (ruleMap, cache) =>
      if (cache.contains(id)) (List(s"found $id in cache"), cache, cache(id))
      else {
        val gen = (id, ruleMap(id)) match {
          case (8, _)                   => rule8Generator
          case (11, _)                  => rule11Generator
          case (_, Single(s))           => singleRegexGenerator(id, s)
          case (_, Combination(groups)) => combinationRegexGenerator(id, groups)
        }
        gen.run(ruleMap, cache).value
      }
  }

  lazy val rule8Generator: RWState[String] = {
    val r8Gen = for {
      r42 <- getRegexGenerator(42)
    } yield s"($r42)+"

    r8Gen flatMap (
      p => addRegexGeneratorToCache(8, p)
    )
  }

  // hack since regex cannot determine an equal number of matches of the two terms
  lazy val rule11Generator: RWState[String] = {
    val r11Gen = for {
      r42 <- getRegexGenerator(42)
      r31 <- getRegexGenerator(31)
    } yield {
      (1 to 20)
        .map {
          i =>
            s"($r42){$i}($r31){$i}"
        }
        .reduce(disjunction)
    }

    r11Gen flatMap (
      p => addRegexGeneratorToCache(11, p)
    )
  }
}
