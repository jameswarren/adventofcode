package jw.aoc2020

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

object Day15 {

  //using mutability for speed
  def history(xs: List[Int]): MMap[Int, Int] = MMap.from(xs.zip(LazyList.from(1)))

  def solve(xs: List[Int], n: Int): Int = {
    val cache = history(xs.dropRight(1))

    @tailrec
    def loop(value: Int, index: Int): Int = {
      if (index == n) value
      else {
        val next = cache.get(value).fold(0) { index - _ }
        cache(value) = index
        loop(next, index + 1)
      }
    }

    loop(xs.last, xs.length)
  }

  def main(args: Array[String]): Unit = {
    println(solve(List(0, 14, 6, 20, 1, 4), 2020))
    println(solve(List(0, 14, 6, 20, 1, 4), 30000000))
  }
}
