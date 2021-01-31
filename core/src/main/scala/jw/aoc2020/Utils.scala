package jw.aoc2020

object Utils {

  def multiLineGroups(lines: Seq[String]): LazyList[List[String]] = {
    val trimmed = lines.dropWhile(_.isEmpty)

    if (trimmed.isEmpty) {
      LazyList.empty[List[String]]
    } else {
      val group = trimmed.takeWhile(_.nonEmpty).toList
      group #:: multiLineGroups(trimmed.dropWhile(_.nonEmpty))
    }
  }
}
