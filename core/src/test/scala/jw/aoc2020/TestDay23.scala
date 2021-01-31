package jw.aoc2020

import jw.aoc2020.Day23._
import org.scalatest.tags.Slow

class TestDay23Part1 extends TestScaffold {
  import Day23Part1._

  val circle: Circle = initCircle(List(3, 8, 9, 1, 2, 5, 4, 6, 7)).get

  "the solution" should "construct an initial circle from the starting values" in {
    circle should equal(
      3 -> Map(
        3 -> 8,
        8 -> 9,
        9 -> 1,
        1 -> 2,
        2 -> 5,
        5 -> 4,
        4 -> 6,
        6 -> 7,
        7 -> 3
      )
    )
  }

  it should "traverse the circle given the order" in {
    traverse(circle._1, circle._2).take(9).toList should equal(List(3, 8, 9, 1, 2, 5, 4, 6, 7))
  }

  it should "find the destination given a desired and restricted values" in {
    destination(2, Set(2, 1, 9), 9) should equal(8)
    destination(4, Set(4, 1, 9), 9) should equal(3)
    destination(5, Set(2, 6, 8), 9) should equal(4)
    destination(1, Set(5, 6, 7), 9) should equal(9)
  }

  it should "step through the example problem" in {
    val stepFn = step(9)
    val iterations = LazyList.iterate(circle)(stepFn)
    val (idx, order) = iterations(10)
    traverse(idx, order).take(9).toList should equal(List(8, 3, 7, 4, 1, 9, 2, 6, 5))
  }

  it should "solve the example problem" in {
    solve(List(3, 8, 9, 1, 2, 5, 4, 6, 7)) should equal("67384529")
  }
}

@Slow
class TestDay23Part2 extends TestScaffold {
  import Day23Part2._

  val circle: Circle = initCircle(List(3, 8, 9, 1, 2, 5, 4, 6, 7)).get

  "the solution" should "construct an initial circle from the starting values" in {
    circle should equal(
      3 -> Map(
        3 -> 8,
        8 -> 9,
        9 -> 1,
        1 -> 2,
        2 -> 5,
        5 -> 4,
        4 -> 6,
        6 -> 7,
        7 -> 10,
        1000000 -> 3
      )
    )
  }

  it should "solve the example problem" in {
    solve(List(3, 8, 9, 1, 2, 5, 4, 6, 7)) should equal(149245887792L)
  }
}
