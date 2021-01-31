package jw.aoc2020

import jw.aoc2020.Day11._

import scala.io.Source

object Day11 {

  case class Grid(cells: Vector[Vector[Cell]]) {
    lazy val rows: Int = cells.length
    lazy val cols: Int = cells.headOption.fold(0)(_.length)
  }

  case class Cell(location: Location, status: Status)

  case class Location(row: Int, col: Int)

  sealed trait Status
  case object Empty extends Status
  case object Free extends Status
  case object Occupied extends Status

  def neighbor(l: Location, rowOffset: Int, colOffset: Int): Location =
    Location(l.row + rowOffset, l.col + colOffset)

  def getCell(l: Location, g: Grid): Cell = g.cells(l.row)(l.col)

  def isInGrid(l: Location, g: Grid): Boolean = l.row >= 0 & l.row < g.rows && l.col >= 0 & l.col < g.cols

  def parseGrid(lines: Seq[String]): Grid = {
    def parseChar(c: Char): Status = c match {
      case 'L' => Free
      case '#' => Occupied
      case _   => Empty
    }

    Grid(for {
      (line, row) <- lines.toVector.zipWithIndex
    } yield {
      for {
        (char, col) <- line.toVector.zipWithIndex
      } yield Cell(Location(row, col), parseChar(char))
    })
  }

  def gridToString(g: Grid): String =
    g.cells
      .map {
        row =>
          row.map {
            cell =>
              cell.status match {
                case Free     => "L"
                case Occupied => "#"
                case _        => "."
              }
          }.mkString
      }
      .mkString("\n")

  def fixedPoint(g: Grid)(f: Grid => Grid): Option[Grid] = {
    val iterations = LazyList.iterate(g)(f)
    val pairs = iterations zip iterations.tail
    pairs.collectFirst { case (g1, g2) if g1 == g2 => g1 }
  }

  def countOccupied(g: Grid): Int = {
    g.cells
      .map {
        row =>
          row.count(_.status == Occupied)
      }
      .sum
  }

  val input: List[String] = Source.fromResource("aoc2020/day11.input").getLines().toList
}

object Day11Part1 {

  def neighborhood(l: Location, g: Grid): Seq[Location] = {
    (for {
      rowOffset <- Range.inclusive(-1, 1)
      colOffset <- Range.inclusive(-1, 1)
      if rowOffset != 0 || colOffset != 0
    } yield {
      neighbor(l, rowOffset, colOffset)
    }) filter {
      loc =>
        isInGrid(loc, g)
    }
  }

  def evolve(g: Grid): Grid = {
    Grid({
      for {
        row <- g.cells
      } yield for {
        cell <- row
      } yield evolveCell(cell, g)
    })
  }

  def evolveCell(c: Cell, g: Grid): Cell = {
    val numOccupied = neighborhood(c.location, g)
      .map(
        l => getCell(l, g)
      )
      .count(_.status == Occupied)
    if (c.status == Free && numOccupied == 0) c.copy(status = Occupied)
    else if (c.status == Occupied && numOccupied >= 4) c.copy(status = Free)
    else c
  }

  def main(args: Array[String]): Unit = {
    val grid = parseGrid(input)
    val fixed = fixedPoint(grid)(evolve)
    val numOccupied = fixed map countOccupied
    println(numOccupied)
  }
}

object Day11Part2 {

  def directionIsClear(loc: Location, g: Grid, rowOffset: Int, colOffset: Int): Boolean = {
    val lineOfSight: LazyList[Location] = LazyList
      .iterate(loc) {
        l =>
          neighbor(l, rowOffset, colOffset)
      }
      .tail
      .takeWhile {
        l =>
          isInGrid(l, g)
      }

    lineOfSight.collectFirst { case l: Location if getCell(l, g).status != Empty => getCell(l, g).status } match {
      case Some(Occupied) => false
      case _              => true
    }
  }

  def evolve(g: Grid): Grid = {
    Grid({
      for {
        row <- g.cells
      } yield for {
        cell <- row
      } yield evolveCell(cell, g)
    })
  }

  def evolveCell(c: Cell, g: Grid): Cell = {
    val numSeen = (for {
      rowOffset <- Range.inclusive(-1, 1)
      colOffset <- Range.inclusive(-1, 1)
      if rowOffset != 0 || colOffset != 0
    } yield directionIsClear(c.location, g, rowOffset, colOffset)).count(!_)

    updateCell(c, numSeen)
  }

  def updateCell(c: Cell, numSeen: Int): Cell = {
    if (c.status == Free && numSeen == 0) c.copy(status = Occupied)
    else if (c.status == Occupied && numSeen >= 5) c.copy(status = Free)
    else c
  }

  def main(args: Array[String]): Unit = {
    val grid = parseGrid(input)
    val fixed = fixedPoint(grid)(evolve)
    val numOccupied = fixed map countOccupied
    println(numOccupied)
  }
}
