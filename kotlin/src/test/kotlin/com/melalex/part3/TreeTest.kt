package com.melalex.part3

import org.junit.Assert.assertEquals
import org.junit.Test

class TreeTest {

    @Test
    fun `should get size`() {
        val actual = tree(tree(10), tree(10)).size()

        assertEquals(3, actual)
    }

    @Test
    fun `should get depth`() {
        val actual = tree(tree(tree(10), tree(10)), tree(10)).depth()

        assertEquals(3, actual)
    }

    @Test
    fun `should get max`() {
        val target = tree(tree(tree(10), tree(20)), tree(tree(40), tree(30)))

        val actual = target.max(Comparator.comparingInt<Int> { i -> i })

        assertEquals(40, actual)
    }

    @Test
    fun `should get map`() {
        val target = tree(tree(tree(10), tree(20)), tree(tree(40), tree(30)))
        val expected = tree(tree(tree("10"), tree("20")), tree(tree("40"), tree("30")))

        val actual = target.map { i -> i.toString() }

        assertEquals(expected, actual)
    }
}