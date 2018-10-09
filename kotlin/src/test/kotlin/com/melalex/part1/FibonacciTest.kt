package com.melalex.part1

import org.junit.Assert
import org.junit.Test

class FibonacciTest {

    private val expectedResults: Map<Int, Long> = mapOf(
            1 to 0L,
            2 to 1L,
            3 to 1L,
            4 to 2L,
            5 to 3L,
            6 to 5L
    )

    @Test
    fun `Should return correct Fibonacci number`() {
        expectedResults.forEach {
            val n = it.key
            val expected = it.value

            Assert.assertEquals("Invalid fibonacci($n) result", expected, fibonacci(n))
        }
    }

    @Test(expected = IllegalArgumentException::class)
    fun `Should throw IllegalArgumentException when argument negative`() {
        fibonacci(-1)
    }
}