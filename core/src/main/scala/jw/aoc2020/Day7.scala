package jw.aoc2020

import cats.Monoid
import jw.aoc2020.Day7._

import scala.io.Source

object Day7 {

  def parse(s: String): Option[(String, Map[String, Int])] = {
    parseStructure(s) map {
      case (source, targets) => source -> parseTargets(targets)
    }
  }

  def parseStructure(s: String): Option[(String, String)] = {
    val structureRegex = "(.*) bags contain (.*)".r

    structureRegex.findFirstMatchIn(s) map {
      str =>
        str.group(1) -> str.group(2)
    }
  }

  def parseTargets(s: String): Map[String, Int] = {
    val targetRegex = "(\\d+) (.*?) bag(s?)".r

    targetRegex
      .findAllMatchIn(s)
      .map {
        tgt =>
          tgt.group(2) -> tgt.group(1).toInt
      }
      .toMap
  }

  val input: List[String] = Source.fromResource("aoc2020/day7.input").getLines().toList

  val inputMap: Map[String, Map[String, Int]] = input.flatMap(parse).toMap
}

object Day7Part1 {

  val inverted: Map[String, List[String]] = Monoid.combineAll(for {
    (source, targets) <- inputMap.toList
    (target, _)       <- targets.toList
  } yield Map(target -> List(source)))

  def totalContainedBy(src: String): Int = {
    def loop(src: String): Set[String] = {
      val next = inverted.getOrElse(src, Nil).toSet
      next ++ next.flatMap(loop)
    }

    loop(src).size
  }

  def main(args: Array[String]): Unit = {
    println(totalContainedBy("shiny gold"))
  }
}

object Day7Part2 {

  def totalContained(src: String): Int = {
    val contained = inputMap.getOrElse(src, Map.empty[String, Int])

    contained.map {
      case (next, count) => count * (totalContained(next) + 1)
    }.sum
  }

  def main(args: Array[String]): Unit = {
    println(totalContained("shiny gold"))
  }
}
