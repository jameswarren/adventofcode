package jw.aoc2020

import atto._
import Atto._

import cats.data.ReaderWriterState
import cats.kernel.Monoid
import cats.syntax.all._

import scala.io.Source

import Day20._
import Symmetry._

object Day20 {
  type Coords = (Int, Int)

  sealed trait Symmetry { def transform(n: Int): Coords => Coords }
  case object SymE extends Symmetry { def transform(n: Int): Coords => Coords = identity[Coords] }
  case object SymA extends Symmetry { def transform(n: Int): Coords => Coords = rot(n) }
  case object SymA2 extends Symmetry { def transform(n: Int): Coords => Coords = SymA.transform(n) compose rot(n) }
  case object SymA3 extends Symmetry { def transform(n: Int): Coords => Coords = SymA2.transform(n) compose rot(n) }
  case object SymB extends Symmetry { def transform(n: Int): Coords => Coords = flip(n) }
  case object SymAB extends Symmetry { def transform(n: Int): Coords => Coords = SymA.transform(n) compose flip(n) }
  case object SymA2B extends Symmetry { def transform(n: Int): Coords => Coords = SymA2.transform(n) compose flip(n) }
  case object SymA3B extends Symmetry { def transform(n: Int): Coords => Coords = SymA3.transform(n) compose flip(n) }

  object Symmetry {
    def flip(n: Int): Coords => Coords = {
      case (i, j) => (i, n - (j + 1))
    }
    def rot(n: Int): Coords => Coords = {
      case (i, j) => (n - (j + 1), i)
    }
    val all: List[Symmetry] = List(SymE, SymA, SymA2, SymA3, SymB, SymAB, SymA2B, SymA3B)
  }

  trait Grid {
    def n: Int
    def entries: Vector[Vector[Boolean]]
    def sym: Symmetry

    def transform(c: Coords): Coords = sym.transform(n)(c)

    def query(c: Coords): Boolean = {
      val (row, col) = transform(c)
      entries(row)(col)
    }

    lazy val oriented: Vector[Vector[Boolean]] = {
      for {
        row <- Vector.from(Range(0, n))
      } yield for {
        col <- Vector.from(Range(0, n))
      } yield query(row, col)
    }

    override def toString: String = oriented.map(Grid.asString).mkString("\n")
  }

  object Grid {
    def asChar(b: Boolean): Char = if (b) '#' else '.'
    def asString(v: Seq[Boolean]): String = v.map(asChar).mkString
  }

  case class Tile(id: Long, entries: Vector[Vector[Boolean]], sym: Symmetry) extends Grid {
    val n: Int = Tile.n
    lazy val top: String = Grid.asString(Range(0, n).map { query(0, _) })
    lazy val bottom: String = Grid.asString(Range(0, n).map { query(n - 1, _) })
    lazy val left: String = Grid.asString(Range(0, n).map { query(_, 0) })
    lazy val right: String = Grid.asString(Range(0, n).map { query(_, n - 1) })
    override lazy val toString: String = s"Tile $id $sym:\n" ++ super.toString
  }

  object Tile {
    val n: Int = 10
    val idParser: Parser[Long] = string("Tile ") ~> long
    val lineParser: Parser[Vector[Boolean]] = manyN(n, char('#') | char('.')).map(
      l => l.map(_ == '#').toVector
    )

    def apply(lines: Seq[String]): Option[Tile] = {
      for {
        id <- idParser.parse(lines.head).option
        grid <- lines.tail traverse (
                  l => lineParser.parse(l).option
                )
      } yield Tile(id, grid.toVector, SymE)
    }

    def boundary(t: Tile): Set[String] = {
      val edges = Set(t.top, t.left, t.right, t.bottom)
      edges ++ edges.map(_.reverse)
    }

    def boundaryMap(t: Tile): Map[String, Set[Long]] = boundary(t).map(_ -> Set(t.id)).toMap

    def edges(t: Tile, connectionMap: Map[String, Set[Long]]): Set[String] = {
      boundary(t) filter {
        b =>
          (connectionMap(b) ++ connectionMap(b.reverse) -- Set(t.id)).isEmpty
      }
    }
  }

  object PuzzleSolver {
    type EdgeMap = Map[Long, Set[String]]
    type TileMap = Map[Long, Tile]
    type Solution = Map[Coords, Tile]
    type RWState[A] = ReaderWriterState[EdgeMap, Unit, (TileMap, Solution), A]

    def solve(tiles: List[Tile], side: Int): Solution = {
      val tileMap = tiles
        .map(
          t => t.id -> t
        )
        .toMap
      val connectionMap = (tiles map Tile.boundaryMap).combineAll
      val edgeMap = (tiles map (
        t => t.id -> Tile.edges(t, connectionMap)
      )).toMap

      val steps: List[RWState[Tile]] = for {
        row <- Range(0, side).toList
        col <- Range(0, side).toList
      } yield findPiece((row, col))

      val (_, solution) = steps.sequence.runS(edgeMap, (tileMap, Map.empty)).value
      solution
    }

    def findPiece(c: Coords): RWState[Tile] = ReaderWriterState {
      case (edgeMap, (tileMap, solution)) =>
        val (row, col) = c
        val leftBoundary = solution.get((row, col - 1)).map(_.right)
        val topBoundary = solution.get((row - 1, col)).map(_.bottom)
        val piece = search(tileMap, edgeMap, leftBoundary, topBoundary).get
        ((), (tileMap - piece.id, solution + (c -> piece)), piece)
    }

    def search(
        tileMap: Map[Long, Tile],
        edgeMap: Map[Long, Set[String]],
        leftBoundary: Option[String],
        topBoundary: Option[String]
    ): Option[Tile] = {

      val candidates: List[Tile] = for {
        tile <- tileMap.values.toList
        sym  <- Symmetry.all
      } yield tile.copy(sym = sym)

      candidates find {
        t =>
          def edgeCondition(s: String, target: Option[String]): Boolean = {
            target.fold(edgeMap(t.id).contains(s)) { _ == s }
          }
          edgeCondition(t.left, leftBoundary) && edgeCondition(t.top, topBoundary)
      }
    }
  }

  case class Picture(n: Int, entries: Vector[Vector[Boolean]], sym: Symmetry) extends Grid

  object Picture {

    def apply(size: Int, solution: Map[Coords, Tile]): Picture = {
      val entries: Vector[Vector[Boolean]] =
        for {
          row <- Range(0, size * 8).toVector
          current: Vector[Boolean] = for {
                                       col <- Range(0, size * 8).toVector
                                     } yield {
                                       val tile = (row / 8, col / 8)
                                       val inner = (row % 8, col % 8) |+| (1, 1)
                                       solution(tile).query(inner)
                                     }
        } yield current

      Picture(size * 8, entries, SymE)
    }

    def monsterCoordinates(p: Picture): Set[Coords] = {
      val matches = for {
        i <- Range(0, p.n - 2)
        j <- Range(0, p.n - 19)
        template = monsterTemplate((i, j))
        if template.forall(p.query)
      } yield template.map(p.transform)

      Monoid[Set[Coords]].combineAll(matches)
    }

    def monsterTemplate(c: Coords): Set[Coords] =
      Set(
        (0, 18),
        (1, 0),
        (1, 5),
        (1, 6),
        (1, 11),
        (1, 12),
        (1, 17),
        (1, 18),
        (1, 19),
        (2, 1),
        (2, 4),
        (2, 7),
        (2, 10),
        (2, 13),
        (2, 16)
      ) map { _ |+| c }

    def totalTrue(p: Picture): Int = p.entries.map(_.count(identity)).sum
  }
}

object Day20Part1 extends App {
  val input = Source.fromResource("./aoc2020/day20.input").getLines().toList
  val tiles = (Utils.multiLineGroups(input) flatMap Tile.apply).toList
  val size = Math.sqrt(tiles.length).toInt
  val solution = PuzzleSolver.solve(tiles, size)

  val corners = List((0, 0), (0, size - 1), (size - 1, 0), (size - 1, size - 1))
  println(
    corners
      .map(
        c => solution(c).id
      )
      .product
  )
}

object Day20Part2 extends App {

  def solve(s: Map[Coords, Tile], size: Int): Int = {
    val picture = Picture(size, s)
    val allPictures = Symmetry.all map {
      s =>
        picture.copy(sym = s)
    }
    val allMonsterCoordinates = (allPictures map Picture.monsterCoordinates).combineAll
    Picture.totalTrue(picture) - allMonsterCoordinates.size
  }

  val input = Source.fromResource("./aoc2020/day20.input").getLines().toList
  val tiles = (Utils.multiLineGroups(input) flatMap Tile.apply).toList
  val size = Math.sqrt(tiles.length).toInt
  val solution = PuzzleSolver.solve(tiles, size)
  println(solve(solution, size))
}
