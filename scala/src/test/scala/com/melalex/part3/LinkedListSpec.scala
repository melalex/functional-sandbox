package com.melalex.part3

import com.melalex.part3.{EmptyLinkedList => Last}
import org.scalatest.FlatSpec

class LinkedListSpec extends FlatSpec {

  "The LinkedList" should "be created" in {
    val actual = LinkedList(0, 1, 2, 3, 4)

    val expected = of(0, of(1, of(2, of(3, of(4, Last)))))

    assert(actual == expected)
  }

  "The LinkedList" should "be reduced" in {
    val target = LinkedList(0, 1, 2, 3, 4)

    assert(10 == target.reduce[Int](0, _ + _))
  }

  private def of[E](first: E, tail: LinkedList[E]) = new NonEmptyLinkedList[E](first, tail)
}
