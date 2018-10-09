package com.melalex.part1

fun fibonacci(n: Int): Long {

    tailrec fun findNext(beforePrevious: Long, previous: Long, count: Int): Long {
        if (count == n) {
            return previous
        }

        return findNext(previous, beforePrevious + previous, count + 1)
    }

    return when {
        n <= 0 -> throw IllegalArgumentException("Invalid input. Expected [ n > 0 ]. Actual [ n = $n ]")
        n == 1 -> 0
        n == 2 -> 1
        else -> findNext(0, 1, 2)
    }
}