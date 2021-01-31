package jw.aoc2020

import jw.aoc2020.Day8._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day8 {

  sealed trait Operation {
    val next: Int = this match {
      case Jump(steps) => steps
      case _           => 1
    }
  }

  case class NoOp(dummy: Int) extends Operation
  case class Jump(steps: Int) extends Operation
  case class Accumulate(increment: Int) extends Operation

  case class State(line: Int, accumulation: Int) {
    def apply(o: Operation): State = o match {
      case NoOp(_)       => copy(line = line + 1)
      case Jump(steps)   => copy(line = line + steps)
      case Accumulate(n) => State(line + 1, accumulation + n)
    }
  }

  def parse(s: String): Option[Operation] = {
    val pattern = "(acc|jmp|nop) \\+?(.*)".r

    pattern.findFirstMatchIn(s) map {
      mtch =>
        mtch.group(1) match {
          case "acc" => Accumulate(mtch.group(2).toInt)
          case "jmp" => Jump(mtch.group(2).toInt)
          case _     => NoOp(mtch.group(2).toInt)
        }
    }
  }

  sealed trait Result
  case object ExceededStack extends Result
  case class InfiniteLoop(acc: Int) extends Result
  case class Success(acc: Int) extends Result

  def run(init: State, ops: Vector[Operation]): Result = {
    val visited = mutable.Set[Int]()

    @tailrec
    def loop(s: State): Result = {
      if (visited.contains(s.line)) InfiniteLoop(s.accumulation)
      else if (s.line > ops.length) ExceededStack
      else if (s.line == ops.length) Success(s.accumulation)
      else {
        visited += s.line
        val op = ops(s.line)
        loop(s(op))
      }
    }

    loop(init)
  }

  val lines: Vector[String] = Source.fromResource("aoc2020/day8.input").getLines().toVector
  val operations: Vector[Operation] = lines flatMap parse
}

object Day8Part1 {

  def main(args: Array[String]): Unit = {
    println(run(State(0, 0), operations))
  }
}

object Day8Part2 {

  val mutations: LazyList[Vector[Operation]] = LazyList.range(0, operations.length).flatMap {
    i =>
      operations(i) match {
        case NoOp(n) => Option(operations.updated(i, Jump(n)))
        case Jump(n) => Option(operations.updated(i, NoOp(n)))
        case _       => None
      }
  }

  def main(args: Array[String]): Unit = {
    val x: LazyList[Result] = mutations.map {
      ops =>
        run(State(0, 0), ops)
    }

    println(x.collectFirst {
      case Success(n) => n
    })
  }
}
