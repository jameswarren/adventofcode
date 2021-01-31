package jw.aoc2020

trait Day17Example {

  val example: List[String] =
    """.#.
      |..#
      |###
      |""".stripMargin.split("\n").toList

  val step1String3D: String =
    """z=-1
      |#..
      |..#
      |.#.
      |
      |z=0
      |#.#
      |.##
      |.#.
      |
      |z=1
      |#..
      |..#
      |.#.
      |""".stripMargin

  val step2String3D: String =
    """z=-2
      |.....
      |.....
      |..#..
      |.....
      |.....
      |
      |z=-1
      |..#..
      |.#..#
      |....#
      |.#...
      |.....
      |
      |z=0
      |##...
      |##...
      |#....
      |....#
      |.###.
      |
      |z=1
      |..#..
      |.#..#
      |....#
      |.#...
      |.....
      |
      |z=2
      |.....
      |.....
      |..#..
      |.....
      |.....
      |""".stripMargin
}

class TestDay17 extends TestScaffold with Day17Example {
  import Day17Part1._

  "the solution" should "construct the initial world from given input" in {
    initWorld(example) should equal(
      Set(
        (1, 0, 0),
        (2, 1, 0),
        (0, 2, 0),
        (1, 2, 0),
        (2, 2, 0)
      )
    )
  }

  it should "render the world" in {
    renderLevel(initWorld(example), 0) should equal(example.mkString("\n"))
  }

  it should "calculate the neighborhood of a coordinate" in {
    neighborhood((0, 0, 0)) should equal(
      Set(
        (-1, -1, -1),
        (-1, -1, 0),
        (-1, -1, 1),
        (-1, 0, -1),
        (-1, 0, 0),
        (-1, 0, 1),
        (-1, 1, -1),
        (-1, 1, 0),
        (-1, 1, 1),
        (0, -1, -1),
        (0, -1, 0),
        (0, -1, 1),
        (0, 0, -1),
        (0, 0, 1),
        (0, 1, -1),
        (0, 1, 0),
        (0, 1, 1),
        (1, -1, -1),
        (1, -1, 0),
        (1, -1, 1),
        (1, 0, -1),
        (1, 0, 0),
        (1, 0, 1),
        (1, 1, -1),
        (1, 1, 0),
        (1, 1, 1)
      )
    )
  }

  it should "solve the example problem" in {
    val world0 = initWorld(example)

    val world1 = evolve(world0)
    renderWorld(world1) should equal(step1String3D)

    val world2 = evolve(world1)
    renderWorld(world2) should equal(step2String3D)

    solve(world0, 6) should equal(112)
  }
}

class TestDay17Part2 extends TestScaffold with Day17Example {
  import Day17Part2._

  "the solution" should "solve the example problem" in {
    val init = initWorld(example)
    solve(init, 6) should equal(848)
  }

  1 -> 2
}
