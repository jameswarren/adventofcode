package jw.aoc2020

object Day25 extends App {

  def loop(x: Long): LazyList[Long] = {
    def f(y: Long): Long = (x * y) % 20201227
    LazyList.iterate(1L)(f)
  }

  def findLoopNum(target: Long): Int = loop(7).zipWithIndex.find(_._1 == target).get._2

  def encryptionKey(public: Long, n: Int): Long = loop(public)(n)

  def solve(public1: Long, public2: Long): Long = {
    val n = findLoopNum(public1)
    encryptionKey(public2, n)
  }

  println(solve(1965712, 19072108))
}
