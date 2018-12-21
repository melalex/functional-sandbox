package com.melalex.part2

import com.melalex.part2.Part2.ExtendedSeq
import org.scalatest.FlatSpec

class Part2Spec extends FlatSpec {

  val expectedResults: Map[Int, Long] = Map(
    1 -> 0L,
    2 -> 1L,
    3 -> 1L,
    4 -> 2L,
    5 -> 3L,
    6 -> 5L
  )

  "The fibonacci" should "return correct Fibonacci number" in {
    expectedResults.foreach { case (k, v) =>
      assert(Part2.fibonacci(k) == v, s"Invalid fibonacci($k) result")
    }
  }

  it should "throw IllegalArgumentException when argument negative" in {
    assertThrows[IllegalArgumentException] {
      Part2.fibonacci(-1)
    }
  }

  "The Seq.isSorted" should "return true when Seq is sorted" in {
    assert(List(1, 2, 2, 3).isSorted((o1: Int, o2: Int) => o1 - o2))
  }

  it should "return false when Seq isn't sorted" in {
    assert(List(1, 2, 2, 3).isSorted((o1: Int, o2: Int) => o1 - o2))
  }
}