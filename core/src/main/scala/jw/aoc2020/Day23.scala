package jw.aoc2020

import jw.aoc2020.Day23._

import scala.collection.mutable.{Map => MMap}

object Day23 {

  type Circle = (Int, MMap[Int, Int])

  def successor(idx: Int, order: MMap[Int, Int]): Int = order.getOrElse(idx, idx + 1)

  def traverse(idx: Int, order: MMap[Int, Int]): LazyList[Int] = LazyList.iterate(idx)(successor(_, order))

  def destination(target: Int, moving: Set[Int], max: Int): Int = {
    def next(i: Int): Int = if (i == 1) max else i - 1
    LazyList
      .iterate(target)(next)
      .tail
      .find(
        n => !moving.contains(n)
      )
      .get
  }

  def step(max: Int): Circle => Circle = {
    case (idx: Int, order: MMap[Int, Int]) =>
      val moving = traverse(idx, order).tail.take(3).toList
      val nextIdx = successor(moving.last, order)
      val target = destination(idx, moving.toSet, max)
      val afterTarget = successor(target, order)
      order ++= Map(idx -> nextIdx, target -> moving.head, moving.last -> afterTarget)
      (nextIdx, order)
  }
}

object Day23Part1 extends App {

  def initCircle(seed: List[Int]): Option[Circle] =
    for {
      start <- seed.headOption
      end   <- seed.lastOption
      pairs = (end -> start) +: seed.zip(seed.tail)
      b = MMap.newBuilder
    } yield (start, MMap.empty ++= pairs.toMap)

  def solve(seed: List[Int]): String = {
    val init = initCircle(seed).get
    val stepFn = step(9)
    val iterations = LazyList.iterate(init)(stepFn)
    val (_, order) = iterations(100)
    traverse(1, order).tail.take(8).mkString
  }

  println(solve(List(1, 3, 7, 8, 2, 6, 4, 9, 5)))
}

object Day23Part2 extends App {

  def initCircle(seed: List[Int]): Option[Circle] =
    for {
      start <- seed.headOption
      end   <- seed.lastOption
      pairs = MMap(end -> 10, 1000000 -> start) ++ seed.zip(seed.tail)
    } yield (start, pairs)

  def solve(seed: List[Int]): Long = {
    val init: (Int, MMap[Int, Int]) = initCircle(seed).get
    val stepFn: ((Int, MMap[Int, Int])) => (Int, MMap[Int, Int]) = step(1000000)
    val iterations = LazyList.iterate(init)(stepFn)
    val (_, order) = iterations(10000000)
    val successors = traverse(1, order).tail.take(2).map(_.toLong).toList
    successors.product
  }

  println(solve(List(1, 3, 7, 8, 2, 6, 4, 9, 5)))
}
