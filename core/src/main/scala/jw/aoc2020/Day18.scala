package jw.aoc2020

import atto.Atto._
import atto._

import scala.io.Source

object Day18Part1 {

  lazy val expr: Parser[Long] = for {
    lhs  <- term
    next <- continuation
  } yield next(lhs)

  lazy val term: Parser[Long] = {
    val p1 = for {
      e <- char('(') ~> expr <~ char(')')
    } yield e

    val p2 = long

    p1 | p2
  }

  lazy val continuation: Parser[Long => Long] = {
    val p1 = for {
      rhs  <- char('+') ~> term
      next <- continuation
    } yield (lhs: Long) => next(lhs + rhs)

    val p2 = for {
      rhs  <- char('*') ~> term
      next <- continuation
    } yield (lhs: Long) => next(lhs * rhs)

    val p3 = ok(identity[Long] _)

    p1 | p2 | p3
  }

  def solve(s: String): Option[Long] = {
    expr.parse(s.replaceAll("\\s+", "")).done.option
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("aoc2020/day18.input").getLines().toList
    println(lines.flatMap(solve).sum)
  }
}

object Day18Part2 {

  lazy val expr: Parser[Long] = for {
    lhs  <- term
    next <- exprContinuation
  } yield next(lhs)

  lazy val exprContinuation: Parser[Long => Long] = {
    val p1 = for {
      rhs  <- char('*') ~> term
      next <- exprContinuation
    } yield (lhs: Long) => next(lhs * rhs)

    val p2 = ok(identity[Long] _)

    p1 | p2
  }

  lazy val term: Parser[Long] = for {
    lhs  <- factor
    next <- termContinuation
  } yield next(lhs)

  lazy val termContinuation: Parser[Long => Long] = {
    val p1 = for {
      rhs  <- char('+') ~> factor
      next <- termContinuation
    } yield (lhs: Long) => next(lhs + rhs)

    val p2 = ok(identity[Long] _)

    p1 | p2
  }

  lazy val factor: Parser[Long] = {
    val p1 = for {
      e <- char('(') ~> expr <~ char(')')
    } yield e

    val p2 = long

    p1 | p2
  }

  def solve(s: String): Option[Long] = {
    expr.parse(s.replaceAll("\\s+", "")).done.option
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("aoc2020/day18.input").getLines().toList
    println(lines.flatMap(solve).sum)
  }
}
