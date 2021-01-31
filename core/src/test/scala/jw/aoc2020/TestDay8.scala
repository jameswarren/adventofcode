package jw.aoc2020

import jw.aoc2020.Day8._

class TestDay8 extends TestScaffold {

  "instructions" should "adjust state accordingly" in {
    val init = State(0, 0)
    init(NoOp(17)) should equal(State(1, 0))
    init(Jump(5)) should equal(State(5, 0))
    init(Accumulate(4)) should equal(State(1, 4))
  }
}
