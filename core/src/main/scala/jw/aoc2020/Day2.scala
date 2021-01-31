package jw.aoc2020

import jw.aoc2020.Day2._

import scala.io.Source
import scala.util.matching.Regex

object Day2 {

  case class PasswordTest(password: String, target: Char, lower: Int, upper: Int)

  val pattern: Regex = "(\\d+)-(\\d+) (.): (.*)".r

  def extractTest(line: String): Option[PasswordTest] = {
    pattern.findFirstMatchIn(line) map {
      matched: Regex.Match =>
        PasswordTest(
          password = matched.group(4),
          target = matched.group(3)(0),
          lower = matched.group(1).toInt,
          upper = matched.group(2).toInt
        )
    }
  }
}

object Day2Part1 {

  def isValid(test: PasswordTest): Boolean = {
    val charCounts: Map[Char, Int] = test
      .password
      .groupMapReduce(identity)(
        _ => 1
      )(_ + _)
    val targetCount = charCounts.getOrElse(test.target, 0)
    targetCount >= test.lower && targetCount <= test.upper
  }

  def main(args: Array[String]): Unit = {
    val candidates: List[String] = Source.fromResource("aoc2020/day2.input").getLines().toList
    val numCorrect = candidates.flatMap(extractTest).count(isValid)
    println(numCorrect)
  }
}

object Day2Part2 {

  def isValid(test: PasswordTest): Boolean = {
    val isLower = test.password.charAt(test.lower - 1) == test.target
    val isUpper = test.password.charAt(test.upper - 1) == test.target
    isLower ^ isUpper
  }

  def main(args: Array[String]): Unit = {
    val candidates: List[String] = Source.fromResource("aoc2020/day2.input").getLines().toList
    val numCorrect = candidates.flatMap(extractTest).count(isValid)
    println(numCorrect)
  }
}
