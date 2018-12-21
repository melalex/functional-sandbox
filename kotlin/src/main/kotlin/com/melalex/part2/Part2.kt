package com.melalex.part2

fun fibonacci(n: Int): Long {

    tailrec fun findNext(beforePrevious: Long, previous: Long, count: Int): Long {
        return if (count == n) previous
        else findNext(previous, beforePrevious + previous, count + 1)
    }

    return when {
        n <= 0 -> throw IllegalArgumentException("Invalid input. Expected [ n > 0 ]. Actual [ n = $n ]")
        n == 1 -> 0
        n == 2 -> 1
        else -> findNext(0, 1, 2)
    }
}

fun <T> Array<T>.isSorted(comparator: Comparator<T>) = (0..size - 2).all {
    comparator.compare(this[it], this[it + 1]) <= 0
}

fun <A, B, C> curry(target: (A, B) -> C) = { a: A -> { b: B -> target(a, b) } }

fun <A, B, C> uncurry(target: (A) -> (B) -> C) = { a: A, b: B -> target(a)(b) }

fun <A, B, C> compose(f: (B) -> C, g: (A) -> B): (A) -> C = { f(g(it)) }
