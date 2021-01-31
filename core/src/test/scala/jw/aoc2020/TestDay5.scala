package jw.aoc2020

import jw.aoc2020.Day5._

class TestDay5 extends TestScaffold {

  "the solution" should "convert strings to values using binary" in {
    convertFromBinary("FBFBBFF") should equal(44)
    convertFromBinary("RLR") should equal(5)
  }

  it should "determine seat ID from input" in {
    seatId("FBFBBFFRLR") should equal(357)
  }
}
