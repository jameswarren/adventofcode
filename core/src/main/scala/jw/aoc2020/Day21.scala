package jw.aoc2020

import atto.Atto._
import atto._
import jw.aoc2020.Day21._

import scala.io.Source

object Day21 {

  def parseLine(s: String): Option[(List[String], Set[String])] = {
    val wordParser = many1(letter).map(_.toList.mkString)
    val ingredientParser = wordParser.sepBy(spaceChar)
    val allergenParser = wordParser.sepBy(string(", "))

    val lineParser = for {
      ingredients <- ingredientParser
      _           <- string(" (contains ")
      allergens   <- allergenParser
    } yield (allergens, ingredients.toSet)

    lineParser.parse(s).done.option
  }

  type ConstraintMap = Map[String, Set[String]]

  def constraintMap(items: List[(List[String], Set[String])]): ConstraintMap = {
    val facts = items.flatMap {
      case (allergens, ingredients) => allergens.map(_ -> ingredients)
    }
    facts.groupMapReduce(_._1)(_._2) { _ intersect _ }
  }

  def identifyIngredients(m: ConstraintMap): Map[String, String] = {

    def identifyKnown(m: ConstraintMap): Option[((String, String), ConstraintMap)] = {
      m.find(_._2.size == 1) map {
        case (allergen, ingredients) =>
          val identified = allergen -> ingredients.head
          val remaining = (m - allergen).map {
            case (as, is) => as -> is.diff(ingredients)
          }
          (identified, remaining)
      }
    }

    LazyList.unfold(m)(identifyKnown).toMap
  }
}

object Day21Part1 extends App {

  def solve(input: List[String]): Int = {
    val items = input flatMap parseLine
    val identified = identifyIngredients(constraintMap(items))
    items.map(_._2.diff(identified.values.toSet).size).sum
  }

  val input: List[String] = Source.fromResource("./aoc2020/day21.input").getLines().toList
  println(solve(input))
}

object Day21Part2 extends App {

  def solve(input: List[String]): String = {
    val items = input flatMap parseLine
    val identified = identifyIngredients(constraintMap(items))
    identified.toList.sortBy(_._1).map(_._2).mkString(",")
  }

  val input: List[String] = Source.fromResource("./aoc2020/day21.input").getLines().toList
  println(solve(input))
}
