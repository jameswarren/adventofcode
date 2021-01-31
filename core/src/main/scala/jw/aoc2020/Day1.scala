package jw.aoc2020

import scala.io.Source

object Day1Part1 {

  def solve(numbers: Seq[Int]): Option[Int] = {
    val intStream: LazyList[Int] = numbers.to(LazyList)
    val setStream: Seq[Set[Int]] = intStream.scanLeft(Set.empty[Int]) { _ + _ }
    val searchStream = setStream.zip(intStream)

    searchStream.collectFirst {
      case (seen, current) if seen.contains(2020 - current) => current * (2020 - current)
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers: LazyList[Int] = Source.fromResource("aoc2020/day1.input").getLines().map(_.toInt).to(LazyList)
    solve(numbers) foreach println
  }
}

object Day1Part2a {

  def createCache(s: Vector[Int]): Map[Int, Seq[(Int, Int)]] = {
    val pairs = for {
      first  <- s.indices
      second <- s.indices.drop(first + 1)
    } yield s(first) -> s(second)
    pairs.groupBy {
      case (a, b) => a + b
    }
  }

  def solve(s: Seq[Int]): Option[Long] = {
    val cache = createCache(s.toVector)

    def isSolution(n: Int): Boolean = cache.get(2020 - n).exists {
      pairs =>
        pairs.exists {
          case (a, b) => a != n && b != n
        }
    }

    s.collectFirst {
      case n: Int if isSolution(n) =>
        val (a, b) = cache(2020 - n).head
        1L * a * b * n
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers: LazyList[Int] = Source.fromResource("aoc2020/day1.input").getLines().map(_.toInt).to(LazyList)
    solve(numbers) foreach println
  }
}
