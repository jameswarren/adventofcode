package jw.aoc2020

import scala.io.Source

object Day17Part1 {

  type Coord = (Int, Int, Int)
  type World = Set[Coord]

  def initWorld(lines: Seq[String]): World = {
    val coords = for {
      (line, y) <- lines.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char == '#'
    } yield (x, y, 0)

    coords.toSet
  }

  def setBounds(xs: Set[Int]): (Int, Int) = (xs.min, xs.max)

  def renderLevel(world: World, zlevel: Int): String = {
    val (minX, maxX) = setBounds(world.map(_._1))
    val (minY, maxY) = setBounds(world.map(_._2))

    val allLines = for {
      y <- Range.inclusive(minY, maxY)

      line = for {
               x <- Range.inclusive(minX, maxX)
               c = if (world.contains(x, y, zlevel)) '#' else '.'
             } yield c
    } yield line.mkString

    allLines.mkString("\n")
  }

  def renderWorld(world: World): String = {
    val (minZ, maxZ) = setBounds(world.map(_._3))

    Range
      .inclusive(minZ, maxZ)
      .map {
        z =>
          s"z=$z\n" + renderLevel(world, z) + "\n"
      }
      .mkString("\n")
  }

  def neighborhood(c: Coord): Set[Coord] = {
    val (x, y, z) = c

    val neighbors = for {
      deltaX <- Range.inclusive(-1, 1)
      deltaY <- Range.inclusive(-1, 1)
      deltaZ <- Range.inclusive(-1, 1)
      if deltaX != 0 || deltaY != 0 || deltaZ != 0
    } yield (x + deltaX, y + deltaY, z + deltaZ)

    neighbors.toSet
  }

  def worldBoundary(world: World): World = {
    for {
      cell     <- world
      neighbor <- neighborhood(cell)
    } yield neighbor
  }

  def evolve(world: World): World = {
    // check active cells first
    val oldGeneration = for {
      cell <- world
      neighbors = neighborhood(cell).intersect(world).size
      if neighbors == 2 || neighbors == 3
    } yield cell

    val newGeneration = for {
      cell <- worldBoundary(world).diff(world)
      neighbors = neighborhood(cell).intersect(world).size
      if neighbors == 3
    } yield cell

    oldGeneration ++ newGeneration
  }

  def solve(world: World, target: Int): Int = {
    val iterations = LazyList.iterate(world)(evolve)
    iterations(target).size
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day17.input").getLines().toList
    val solution = solve(initWorld(input), 6)
    println(solution)
  }
}

object Day17Part2 {

  type Coord = (Int, Int, Int, Int)
  type World = Set[Coord]

  def initWorld(lines: Seq[String]): World = {
    val coords = for {
      (line, y) <- lines.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char == '#'
    } yield (0, x, y, 0)

    coords.toSet
  }

  def neighborhood(c: Coord): Set[Coord] = {
    val (w, x, y, z) = c

    val neighbors = for {
      deltaW <- Range.inclusive(-1, 1)
      deltaX <- Range.inclusive(-1, 1)
      deltaY <- Range.inclusive(-1, 1)
      deltaZ <- Range.inclusive(-1, 1)
      if deltaW != 0 || deltaX != 0 || deltaY != 0 || deltaZ != 0
    } yield (w + deltaW, x + deltaX, y + deltaY, z + deltaZ)

    neighbors.toSet
  }

  def worldBoundary(world: World): World = {
    for {
      cell     <- world
      neighbor <- neighborhood(cell)
    } yield neighbor
  }

  def evolve(world: World): World = {
    // check active cells first
    val oldGeneration = for {
      cell <- world
      neighbors = neighborhood(cell).intersect(world).size
      if neighbors == 2 || neighbors == 3
    } yield cell

    val newGeneration = for {
      cell <- worldBoundary(world).diff(world)
      neighbors = neighborhood(cell).intersect(world).size
      if neighbors == 3
    } yield cell

    oldGeneration ++ newGeneration
  }

  def solve(world: World, target: Int): Int = {
    val iterations = LazyList.iterate(world)(evolve)
    iterations(target).size
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("aoc2020/day17.input").getLines().toList
    val solution = solve(initWorld(input), 6)
    println(solution)
  }
}
