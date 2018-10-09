package com.melalex.part1

import scala.annotation.tailrec

object Fibonacci {

  def fibonacciOf(n: Int): Long = {

    @tailrec
    def findNext(beforePrevious: Long, previous: Long, count: Int): Long =
      if (count == n) previous
      else findNext(previous, beforePrevious + previous, count + 1)

    n match {
      case x if x <= 0 => throw new IllegalArgumentException(s"Invalid input. Expected [ n > 0 ]. Actual [ n = $n ]")
      case 1 => 0
      case 2 => 1
      case _ => findNext(0, 1, 2)
    }
  }
}