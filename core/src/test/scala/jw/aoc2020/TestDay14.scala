package jw.aoc2020

import jw.aoc2020.Day14._

class TestDay14 extends TestScaffold {

  "the solution" should "convert strings of 0's and 1's to a long value" in {
    val input = "000000000000000000000000000001100101"
    valueFromBinaryString(input) should equal(101)
  }

  it should "extract the bit AND mask from a generic mask" in {
    val input = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    val expected = "111111111111111111111111111111111101"
    extractBitandMask(input) should equal(expected)
  }

  it should "extract the bit OR mask from a generic mask" in {
    val input = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    val expected = "000000000000000000000000000001000000"
    extractBitorMask(input) should equal(expected)
  }

  it should "apply masks according to the example" in {
    val input = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    val andMask = valueFromBinaryString(extractBitandMask(input))
    val orMask = valueFromBinaryString(extractBitorMask(input))
    applyMasks(11, andMask, orMask) should equal(73)
  }

  it should "extract groups of related lines from input" in {
    val input =
      """mask = 011011X11X11100101XX0XX0100100000X0X
        |mem[48514] = 171994
        |mem[14856] = 472531
        |mem[57899] = 15860
        |mem[41284] = 37917047
        |mem[8885] = 893069967
        |mem[28070] = 861473
        |mask = X1X0111010X011100101001XX1XX111X0X01
        |mem[6533] = 1380
        |mem[24785] = 232003103
        |mem[39561] = 1813
        |mem[56060] = 528844
        |mem[12033] = 500106
        |mem[42461] = 942
        |mask = 011011X1110110X10100X0110100101X0111
        |mem[46150] = 77769198
        |mem[60284] = 46877
        |mem[4481] = 183608702
        |""".stripMargin.split("\n").toList
    extractGroups(input).length should equal(3)
  }
}

class TestDay14Part1 extends TestScaffold {
  import Day14Part1._

  it should "create the corresponding map from a group" in {
    val input =
      """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        |mem[8] = 11
        |mem[7] = 101
        |mem[8] = 0
        |""".stripMargin.split("\n").toList

    processGroup(input) should equal(Map(8 -> 64, 7 -> 101))
  }
}

class TestDay14Part2 extends TestScaffold {
  import Day14Part2._

  "the solution" should "extract the non-floating part of the mask" in {
    basicMask("000000000000000000000000000000X1001X") should equal(18)
  }

  it should "extract indices of the floating bits" in {
    floatingBits("000000000000000000000000000000X1001X") should equal(List(0, 5))
  }

  it should "set a specific bit" in {
    setBitToZero(31, 0) should equal(30)
    setBitToOne(0, 4) should equal(16)
    setBitToOne(0, 34) should equal(17179869184L)
  }

  it should "create a sequence of all potential values given floating bits" in {
    floatSequence(26, List(0, 5)) should contain theSameElementsAs List(26, 27, 58, 59)
  }

  it should "process groups correctly" in {
    val group1 = List("mask = 000000000000000000000000000000X1001X", "mem[42] = 100")
    processGroup(group1) should equal(Map(26 -> 100, 27 -> 100, 58 -> 100, 59 -> 100))

    val group2 = List("mask = 00000000000000000000000000000000X0XX", "mem[26] = 1")
    processGroup(group2) should equal(Map(16 -> 1, 17 -> 1, 18 -> 1, 19 -> 1, 24 -> 1, 25 -> 1, 26 -> 1, 27 -> 1))
  }

  it should "solve the example problem" in {
    val input =
      """mask = 000000000000000000000000000000X1001X
        |mem[42] = 100
        |mask = 00000000000000000000000000000000X0XX
        |mem[26] = 1
        |""".stripMargin.split("\n").toList

    aggregateGroups(extractGroups(input))(processGroup).values.sum should equal(208)
  }

  it should "solve group from problem input" in {
    val input =
      """mask = 011011X11X11100101XX0XX0100100000X0X
        |mem[48514] = 171994
        |""".stripMargin.split("\n").toList

    val mask = input.head.drop(7)
    val basic = basicMask(mask)

    println(mask)
    println(basic.toBinaryString, basic)
    println(floatingBits(mask))
    println(parseAssignmentLine(input(1)))
  }
}
