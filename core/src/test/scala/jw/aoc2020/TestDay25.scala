package jw.aoc2020

import jw.aoc2020.Day25._

class TestDay25 extends TestScaffold {

  "the solution" should "find the number of loops given a target" in {
    findLoopNum(5764801) should equal(8)
    findLoopNum(17807724) should equal(11)
  }

  it should "calculate the encryption key given a public key and loop number" in {
    encryptionKey(5764801, 11) should equal(14897079)
    encryptionKey(17807724, 8) should equal(14897079)
  }

  it should "solve the example problem" in {
    solve(5764801, 17807724) should equal(14897079)
  }
}
