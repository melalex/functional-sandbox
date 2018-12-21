package com.melalex.part2

import org.junit.Assert.*
import org.junit.Test

class Part2Test {

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

            assertEquals("Invalid fibonacci($n) result", expected, fibonacci(n))
        }
    }

    @Test(expected = IllegalArgumentException::class)
    fun `Should throw IllegalArgumentException when argument negative`() {
        fibonacci(-1)
    }

    @Test
    fun `Should return true on sorted Array`() {
        val target = arrayOf(1, 2, 2, 3)

        assertTrue(target.isSorted(Comparator { o1, o2 -> o1 - o2 }))
    }

    @Test
    fun `Should return false on not sorted Array`() {
        val target = arrayOf(1, 2, 1, 3)

        assertFalse(target.isSorted(Comparator { o1, o2 -> o1 - o2 }))
    }
}