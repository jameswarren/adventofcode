package jw.aoc2020

import cats.Monoid
import jw.aoc2020.Day4._

import scala.io.Source
import scala.util.Try

object Day4 {

  def aggregate(rows: List[String]): Map[String, String] = {
    def processRow(row: String): Map[String, String] = {
      val fields = row.trim.split("\\s+")
      fields.map {
        pair =>
          val Array(k, v) = pair.split(':')
          k -> v
      }.toMap
    }

    Monoid.combineAll(rows.map(processRow))
  }
}

object Day4Part1 {

  def isValid(passport: Map[String, String]): Boolean = {
    val required = Set("eyr", "hcl", "byr", "pid", "iyr", "hgt", "ecl")
    required.diff(passport.keySet).isEmpty
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day4.input").getLines().toList
    val data = Utils.multiLineGroups(input)
    val passports = data map aggregate
    println(passports.count(isValid))
  }
}

object Day4Part2 {

  case class Passport(fields: Map[String, String]) {
    def checkYear(key: String, min: Int, max: Int): Option[Boolean] =
      for {
        field <- fields.get(key)
        year  <- Try(field.toInt).toOption
      } yield year >= min && year <= max

    val checkHair: Option[Boolean] =
      for {
        field <- fields.get("hcl")
      } yield field.matches("#[0-9a-f]{6}")

    val checkEye: Option[Boolean] = {
      val validColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      for {
        field <- fields.get("ecl")
      } yield validColors.contains(field.toLowerCase)
    }

    val checkPassportId: Option[Boolean] =
      for {
        field <- fields.get("pid")
      } yield field.matches("\\d{9}")

    val checkHeight: Option[Boolean] =
      for {
        field <- fields.get("hgt")
        valid <- checkHeightValue(field)
      } yield valid

    def checkHeightValue(s: String): Option[Boolean] = {
      val cmMatcher = "(\\d+)((cm)|(in))".r
      cmMatcher.findFirstMatchIn(s) map {
        m =>
          val n = m.group(1).toInt
          if (m.group(2) == "cm") {
            n >= 150 && n <= 193
          } else if (m.group(2) == "in") {
            n >= 59 && n <= 76
          } else false
      }
    }

    val isValid: Boolean = {
      List(
        checkHeight,
        checkPassportId,
        checkEye,
        checkHair,
        checkYear("byr", 1920, 2002),
        checkYear("iyr", 2010, 2020),
        checkYear("eyr", 2020, 2030)
      ).forall { _.exists(identity) }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day4.input").getLines().toList
    val data = Utils.multiLineGroups(input)
    val passports = data.map(
      d => Passport(aggregate(d))
    )
    println(passports.count(_.isValid))
  }
}
