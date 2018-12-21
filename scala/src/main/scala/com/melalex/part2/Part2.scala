package com.melalex.part2

import java.util.Comparator

import scala.annotation.tailrec

object Part2 {

  def fibonacci(n: Int): Long = {

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

  def curry[A, B, C](target: (A, B) => C): A => B => C = a => b => target(a, b)

  def uncurry[A, B, C](target: A => B => C): (A, B) => C = (a, b) => target(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  implicit class ExtendedSeq[A](seq: Seq[A]) {

    def isSorted(comparator: Comparator[A]): Boolean = {

      @tailrec
      def compareNeighbors(i: Int, previousResult: Boolean): Boolean =
        if (!previousResult) false
        else if (i >= seq.size) true
        else compareNeighbors(i + 1, comparator.compare(seq(i - 1), seq(i)) <= 0)

      compareNeighbors(1, previousResult = true)
    }
  }

}
