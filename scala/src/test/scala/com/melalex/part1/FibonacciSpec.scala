package com.melalex.part1

import org.scalatest.FlatSpec

class FibonacciSpec extends FlatSpec {

  val expectedResults: Map[Int, Long] = Map(
    1 -> 0L,
    2 -> 1L,
    3 -> 1L,
    4 -> 2L,
    5 -> 3L,
    6 -> 5L
  )

  "Fibonacci" should "return correct Fibonacci number" in {
    expectedResults.foreach { case(k, v) =>
      assert(Fibonacci.fibonacciOf(k) == v, s"Invalid fibonacci($k) result")
    }
  }

  it should "throw IllegalArgumentException when argument negative" in {
    assertThrows[IllegalArgumentException] {
      Fibonacci.fibonacciOf(-1)
    }
  }
}