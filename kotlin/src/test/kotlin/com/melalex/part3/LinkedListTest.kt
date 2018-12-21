package com.melalex.part3

import org.junit.Assert.assertEquals
import org.junit.Test

class LinkedListTest {

    @Test
    fun `should create list of integers`() {
        val actual = LinkedList.of(0, 1, 2, 3, 4)

        assertEquals(listOf(0, 1, 2, 3, 4), actual.iterator().asSequence().toList())
    }

    @Test
    fun `should reduce`() {
        val target = LinkedList.of(0, 1, 2, 3, 4)

        val actual = target.reduce(0) { result, it ->
            result + it
        }

        assertEquals(10, actual)
    }
}