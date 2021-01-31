package jw.aoc2020

import jw.aoc2020.Day12._

import scala.annotation.tailrec
import scala.io.Source

//scalastyle:off cyclomatic.complexity
object Day12 {

  sealed trait Command
  case object Left extends Command
  case object Right extends Command
  case object Forward extends Command

  sealed trait Direction extends Command
  case object North extends Direction
  case object South extends Direction
  case object West extends Direction
  case object East extends Direction

  def parseInstruction(line: String): Option[(Command, Int)] = {
    val pattern = "([NWESFLR])(\\d+)".r
    pattern.findFirstMatchIn(line) map {
      m =>
        val amount = m.group(2).toInt
        val instruction = m.group(1) match {
          case "N" => North
          case "S" => South
          case "E" => East
          case "W" => West
          case "L" => Left
          case "R" => Right
          case "F" => Forward
        }
        instruction -> amount
    }
  }

  val instructions: List[(Command, Int)] =
    Source.fromResource("aoc2020/day12.input").getLines().toList.flatMap(parseInstruction)
}

object Day12Part1 {

  case class Ship(x: Int, y: Int, heading: Direction)

  @tailrec
  def move(ship: Ship, command: Command, amount: Int): Ship = command match {
    case North => ship.copy(y = ship.y + amount)
    case South => ship.copy(y = ship.y - amount)
    case West  => ship.copy(x = ship.x - amount)
    case East  => ship.copy(x = ship.x + amount)

    case Forward => move(ship, ship.heading, amount)
    case Right   => ship.copy(heading = LazyList.iterate(ship.heading)(rotateClockwise90)(amount / 90))
    case Left    => ship.copy(heading = LazyList.iterate(ship.heading)(rotateClockwise90)((360 - amount) / 90))
  }

  def rotateClockwise90(d: Direction): Direction = d match {
    case North => East
    case East  => South
    case South => West
    case West  => North
  }

  def distance(ship: Ship): Int = math.abs(ship.x) + math.abs(ship.y)

  def main(args: Array[String]): Unit = {
    val initState = Ship(0, 0, East)
    val finalState = instructions.foldLeft(initState) {
      case (ship, (command, amount)) => move(ship, command, amount)
    }
    println(distance(finalState))
  }
}

object Day12Part2 {

  case class Location(x: Int, y: Int) {
    def mult(scale: Int): Location = Location(x * scale, y * scale)
    def add(other: Location): Location = Location(x + other.x, y + other.y)
    lazy val dist: Int = math.abs(x) + math.abs(y)
  }

  case class Ship(pos: Location, waypoint: Location)

  def move(ship: Ship, command: Command, amount: Int): Ship = command match {
    case North => ship.copy(waypoint = ship.waypoint.add(Location(0, amount)))
    case South => ship.copy(waypoint = ship.waypoint.add(Location(0, -amount)))
    case West  => ship.copy(waypoint = ship.waypoint.add(Location(-amount, 0)))
    case East  => ship.copy(waypoint = ship.waypoint.add(Location(amount, 0)))

    case Forward => ship.copy(pos = ship.pos.add(ship.waypoint.mult(amount)))
    case Right   => ship.copy(waypoint = LazyList.iterate(ship.waypoint)(rotateClockwise90)(amount / 90))
    case Left    => ship.copy(waypoint = LazyList.iterate(ship.waypoint)(rotateClockwise90)((360 - amount) / 90))
  }

  def rotateClockwise90(l: Location): Location = Location(l.y, -l.x)

  def main(args: Array[String]): Unit = {
    val initState = Ship(Location(0, 0), Location(10, 1))
    val finalState = instructions.foldLeft(initState) {
      case (ship, (command, amount)) => move(ship, command, amount)
    }
    println(finalState.pos.dist)
  }
}
